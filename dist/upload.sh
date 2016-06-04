#!/bin/bash

chmod a+x ./dist/dropbox_uploader.sh

if [ -n "${TRAVIS_TAG}" ]; then
    ./dist/dropbox_uploader.sh -f ./dropbox_uploader -p upload target/*-one-jar.jar cheminotdb-${TRAVIS_TAG}.jar
    curl -H "Content-Type: application/json" --data '{"docker_tag": "${TRAVIS_TAG}"}' -X POST https://registry.hub.docker.com/u/cheminotorg/cheminot.db/trigger/$DOCKER_TOKEN/
fi

./dist/dropbox_uploader.sh -f ./dropbox_uploader -p upload target/*-one-jar.jar cheminotdb-latest.jar

curl -H "Content-Type: application/json" --data '{"source_type": "Branch", "source_name": "master"}' -X POST https://registry.hub.docker.com/u/cheminotorg/cheminot.db/trigger/$DOCKER_TOKEN/
