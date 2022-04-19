# Build the Docker image
build:
  nix run .#ociImage.copyToDockerDaemon

# Push to docker hub
release: 
  docker push ghcr.io/simmsb/calamity-bot
