#!/bin/bash

chmod a+x ./dist/dropbox_uploader.sh

if [ -n "${TRAVIS_TAG}" ]; then
    ./dist/dropbox_uploader.sh -f ./dropbox_uploader -p upload target/*-one-jar.jar cheminotdb-${TRAVIS_TAG}.jar
fi

./dist/dropbox_uploader.sh -f ./dropbox_uploader -p upload target/*-one-jar.jar cheminotdb-latest.jar

