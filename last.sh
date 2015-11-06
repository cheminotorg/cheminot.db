#!/bin/bash

VERSION=`date +%Y%m%d%H%M%S`

cd gtfs/

rm -r $VERSION

mkdir $VERSION

cd $VERSION

mkdir ter

cd ter

wget http://medias.sncf.com/sncfcom/open-data/gtfs/export-TER-GTFS-LAST.zip

unzip export-TER-GTFS-LAST.zip

cd ..

mkdir trans

cd trans

wget http://medias.sncf.com/sncfcom/open-data/gtfs/gtfs-lines-last.zip

unzip gtfs-lines-last.zip

cd ..

mkdir inter

cd inter

wget http://medias.sncf.com/sncfcom/open-data/gtfs/export-INTERCITES-GTFS-LAST.zip

unzip export-INTERCITES-GTFS-LAST.zip
