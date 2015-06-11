#!/bin/bash

rm cheminotdb.zip
cd db
zip -r cheminotdb.zip $1
ssh sre@cheminot.org 'rm /home/sre/sites/cheminot.org/app/cheminotdb/cheminotdb.zip'
scp cheminotdb.zip sre@cheminot.org:/home/sre/sites/cheminot.org/app/cheminotdb/
rm cheminotdb.zip
