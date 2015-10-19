#!/bin/bash

cd db/$1
mkdir cheminotdb
cp * cheminotdb/
zip -r cheminotdb.zip cheminotdb
dropbox_uploader.sh -p upload cheminotdb.zip cheminotdb-latest.zip
dropbox_uploader.sh share cheminotdb-latest.zip
rm -r cheminotdb/
