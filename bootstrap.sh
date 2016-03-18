#!/usr/bin/env bash
docker build \
       --pull=true \
       --no-cache=true \
       --force-rm=true \
       -t reg001.local:5000/haystack \
       -f Dockerfile.fedora .

docker push reg001.local:5000/haystack
