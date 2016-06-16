FROM debian:8.3

MAINTAINER Sebastien Renault <srenault.contact@gmail.com>

RUN apt-get -y update && apt-get -y upgrade

RUN apt-get -y install apt-transport-https ca-certificates

RUN apt-get -y install wget

RUN apt-get -y install openjdk-7-jre

RUN echo "deb https://apt.dockerproject.org/repo debian-jessie main" >> /etc/apt/sources.list.d/backports.list

RUN apt-get -y update

RUN apt-get -y --force-yes install docker-engine

WORKDIR /app

RUN wget https://www.dropbox.com/s/arawxpbz6bwyhbx/cheminotcli-latest.jar?dl=1 -O cheminotcli.jar

ENTRYPOINT [ "java", "-jar", "cheminotcli.jar"]
