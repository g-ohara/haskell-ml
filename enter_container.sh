#!/usr/bin/bash

docker compose up -d
xhost +local:
docker compose exec haskell bash
xhost -local:
