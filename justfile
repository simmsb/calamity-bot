# Build the Docker image
build:
  nix build .#ociImage
  docker load < result
  # nix run .#ociImage.copyToDockerDaemon

# Push to docker hub
release: 
  docker push ghcr.io/simmsb/calamity-bot
