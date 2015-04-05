# cheminot.db

## Build
Download from this [page](https://ressources.data.sncf.com/explore/dataset/sncf-ter-gtfs) the GTFS SNCF TER data to `gtfs/<yyyyMMddHHmmss>/ter` directory.

In order to build `cheminot.db`:
```shell
sbt run
```

## Generate protobuf cpp/java code

```shell
cd protobuf
protoc --java_out java --cpp_out cpp cheminotBuf.proto`
```
