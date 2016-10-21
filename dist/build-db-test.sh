#!/bin/bash

rm -r db
rm -r gtfs
sbt run
cd db/current/
tar zcvf cheminotdb-test.tar.gz cheminot.db
dropbox_uploader.sh -p upload cheminotdb-test.tar.gz cheminotdb-test.tar.gz
rm cheminotdb-test.tar.gz
