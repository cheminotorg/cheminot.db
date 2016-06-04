FROM debian:8.3

MAINTAINER Sebastien Renault <srenault.contact@gmail.com>

RUN apt-get -y update && apt-get -y upgrade

RUN apt-get -y install apt-transport-https ca-certificates

RUN apt-get -y install wget

RUN apt-get -y install openjdk-7-jre

WORKDIR /app

RUN wget https://www.dropbox.com/s/arawxpbz6bwyhbx/cheminotdb-latest.jar?dl=1 -O cheminotdb.jar

ENTRYPOINT [ "java", "-jar", "cheminotdb.jar"]
