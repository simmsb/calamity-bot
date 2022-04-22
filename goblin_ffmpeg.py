import ffmpeg
from itertools import count

width = 800
height = 800

src = ffmpeg.input("lol.mp4")
goblin = ffmpeg.input("goblin.mp4")

def adapt_multi_output(s):
    s = s.filter_multi_output('split')
    def inner():
        for i in count():
            yield s[i]
    inst = inner()
    return lambda: next(inst)

vid = adapt_multi_output(src.filter('scale', width, height).filter('setsar', '1/1'))
goblin_vid = adapt_multi_output(goblin.filter('scale', width, height).filter('setsar', '1/1'))

start = 3.0
step_duration = 0.092

initial_audio = src.audio.filter("atrim", end=start)
initial_video = vid().trim(end=start)

fwd = adapt_multi_output(vid().trim(start=start, duration=step_duration).setpts("PTS-STARTPTS"))
rev = adapt_multi_output(fwd().filter("reverse").setpts("PTS-STARTPTS"))

fwd_slow = fwd().setpts("PTS*0.538")
rev_slow = rev().setpts("PTS*0.538")

funny = ffmpeg.concat(fwd(), rev(), fwd_slow, rev(), fwd(), rev_slow)
funny_duration = 0.092 * 4 + 0.171 * 2
goblin_audio_funny = goblin.audio.filter('atrim', end=funny_duration).filter("asetpts", "PTS-STARTPTS")

goblin_remainder_video = goblin_vid().trim(start=funny_duration).setpts("PTS-STARTPTS")
goblin_remainder_audio = goblin.audio.filter('atrim', start=funny_duration).filter("asetpts", "PTS-STARTPTS")

final_audio = ffmpeg.concat(initial_audio, goblin_audio_funny, goblin_remainder_audio, v=0, a=1)
final_video = ffmpeg.concat(initial_video, funny, goblin_remainder_video, v=1, a=0)

out = ffmpeg.output(final_video, final_audio, "haha.mp4", vsync="vfr")

print(" ".join(f"\"{x}\"" for x in out.compile()))
