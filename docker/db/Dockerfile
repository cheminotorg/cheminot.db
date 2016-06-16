FROM debian:8.3

MAINTAINER Sebastien Renault <srenault.contact@gmail.com>

RUN apt-get -y update && apt-get -y upgrade

RUN apt-get -y install wget

RUN apt-get -y install apt-transport-https ca-certificates

RUN wget -O - https://debian.neo4j.org/neotechnology.gpg.key | apt-key add -

RUN echo 'deb http://debian.neo4j.org/repo stable/' > /tmp/neo4j.list

RUN mv /tmp/neo4j.list /etc/apt/sources.list.d

RUN apt-get -y update

RUN apt-get -y install neo4j=2.3.1

WORKDIR /etc/neo4j/

RUN cp neo4j-server.properties neo4j-server.properties.bk

RUN awk -F"=" -v value="false" 'BEGIN {OFS="=";} /dbms.security.auth_enabled/{$2=value;print;next}1' neo4j-server.properties.bk > neo4j-server.properties

RUN rm neo4j-server.properties.bk

RUN cp neo4j-server.properties neo4j-server.properties.bk

RUN awk -F"=" -v value="false" 'BEGIN {OFS="=";} /org.neo4j.server.webserver.https.enabled/{$2=value;print;next}1' neo4j-server.properties.bk > neo4j-server.properties

RUN rm neo4j-server.properties.bk

RUN echo "org.neo4j.server.webserver.address=0.0.0.0" >> neo4j-server.properties

WORKDIR /app

RUN wget https://raw.githubusercontent.com/cheminotorg/cheminot.db/master/scripts/neo4j -O neo4j

RUN chmod +x ./neo4j

CMD ["--path", "/data/world"]

ENTRYPOINT ["/app/neo4j"]
