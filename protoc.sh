#!/bin/bash

SRC_DIR="protobuf/"

CPP_DST_DIR="protobuf/cpp"

JAVA_DST_DIR="protobuf/java"

rm -rf $CPP_DST_DIR

mkdir -p $CPP_DST_DIR

rm -rf $JAVA_DST_DIR

mkdir -p $JAVA_DST_DIR

protoc -I=$SRC_DIR --cpp_out=$CPP_DST_DIR --java_out=$JAVA_DST_DIR $SRC_DIR/cheminotBuf.proto

cp $CPP_DST_DIR/* ../cheminot.c/src/protobuf/

cp $JAVA_DST_DIR/m/cheminot/data/* src/main/scala/protobuf/
