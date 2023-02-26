-- | Polysemy prometheus
module Polysemy.Prometheus (
  runMetricsPrometheusIO,
) where

import Calamity.Metrics.Eff
import Calamity.Metrics.Internal
import Control.Concurrent (forkIO)
import Data.HashMap.Strict as H
import Data.IORef
import Data.Text qualified as T
import Data.Vector qualified as V
import Polysemy qualified as P
import Polysemy.AtomicState qualified as P
import System.Metrics.Prometheus.Http.Scrape qualified as M
import System.Metrics.Prometheus.Metric.Counter qualified as M
import System.Metrics.Prometheus.Metric.Gauge qualified as M
import System.Metrics.Prometheus.Metric.Histogram qualified as M
import System.Metrics.Prometheus.MetricId qualified as M
import System.Metrics.Prometheus.Registry qualified as MR

data PrometheusMetricsState = PrometheusMetricsState
  { registry :: MR.Registry
  , registeredCounters :: H.HashMap (T.Text, [(T.Text, T.Text)]) Counter
  , counters :: V.Vector M.Counter
  , registeredGauges :: H.HashMap (T.Text, [(T.Text, T.Text)]) Gauge
  , gauges :: V.Vector M.Gauge
  , registeredHistograms :: H.HashMap (T.Text, [(T.Text, T.Text)], [Double]) Histogram
  , histograms :: V.Vector M.Histogram
  }

translateH :: M.HistogramSample -> HistogramSample
translateH M.HistogramSample {M.histBuckets, M.histSum, M.histCount} = HistogramSample histBuckets histSum histCount

runMetricsPrometheusIO :: P.Member (P.Embed IO) r => P.Sem (MetricEff ': r) a -> P.Sem r a
runMetricsPrometheusIO m = do
  var <- P.embed $ newIORef $ PrometheusMetricsState MR.new mempty mempty mempty mempty mempty mempty
  P.embed . forkIO $ M.serveMetrics 6699 ["metrics"] (readIORef var >>= MR.sample . registry)
  P.runAtomicStateIORef var $
    P.reinterpret
      ( \case
          RegisterCounter name labels -> do
            state <- P.atomicGet
            case H.lookup (name, labels) (registeredCounters state) of
              Just counter -> pure counter
              Nothing -> do
                (counterP, registry') <- P.embed $ MR.registerCounter (M.Name name) (M.fromList labels) (registry state)
                let idx = V.length $ counters state
                let counter = Counter idx
                P.atomicModify
                  ( \state ->
                      state
                        { registry = registry'
                        , counters = V.snoc (counters state) counterP
                        , registeredCounters = H.insert (name, labels) counter (registeredCounters state)
                        }
                  )
                pure counter
          RegisterGauge name labels -> do
            state <- P.atomicGet
            case H.lookup (name, labels) (registeredGauges state) of
              Just gauge -> pure gauge
              Nothing -> do
                (gaugeP, registry') <- P.embed $ MR.registerGauge (M.Name name) (M.fromList labels) (registry state)
                let idx = V.length $ gauges state
                let gauge = Gauge idx
                P.atomicModify
                  ( \state ->
                      state
                        { registry = registry'
                        , gauges = V.snoc (gauges state) gaugeP
                        , registeredGauges = H.insert (name, labels) gauge (registeredGauges state)
                        }
                  )
                pure gauge
          RegisterHistogram name labels bounds -> do
            state <- P.atomicGet
            case H.lookup (name, labels, bounds) (registeredHistograms state) of
              Just histogram -> pure histogram
              Nothing -> do
                (histogramP, registry') <- P.embed $ MR.registerHistogram (M.Name name) (M.fromList labels) bounds (registry state)
                let idx = V.length $ histograms state
                let histogram = Histogram idx
                P.atomicModify
                  ( \state ->
                      state
                        { registry = registry'
                        , histograms = V.snoc (histograms state) histogramP
                        , registeredHistograms = H.insert (name, labels, bounds) histogram (registeredHistograms state)
                        }
                  )
                pure histogram
          AddCounter by (Counter id) -> P.atomicGets counters >>= (M.unCounterSample <$>) . P.embed . M.addAndSample by . (V.! id)
          ModifyGauge f (Gauge id) -> P.atomicGets gauges >>= (M.unGaugeSample <$>) . P.embed . M.modifyAndSample f . (V.! id)
          ObserveHistogram val (Histogram id) -> P.atomicGets histograms >>= (translateH <$>) . P.embed . M.observeAndSample val . (V.! id)
      )
      m
