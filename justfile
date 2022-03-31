# Build the Docker image
build:
  nix build .#ociImage
  docker load < result

# Push to docker hub
release: 
  docker push ghcr.io/simmsb/calamity-bot
