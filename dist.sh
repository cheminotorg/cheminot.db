#!/bin/bash

cd db/$1
mkdir cheminotdb
cp stops_ttree.json cheminotdb/stops_ttree.json
cp calendardates-$1 cheminotdb/calendardates
cp cheminot-$1.db cheminotdb/cheminot.db
cp graph-$1 cheminotdb/graph
zip -r cheminotdb.zip cheminotdb
dropbox_uploader.sh -p upload cheminotdb.zip cheminotdb-latest.zip
dropbox_uploader.sh share cheminotdb-latest.zip
rm -r cheminotdb/
