#!/usr/bin/bash

docker compose up -d
xhost +local:
docker compose exec lhaskell bash
xhost -local:
