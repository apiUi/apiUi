FROM debian:latest
MAINTAINER Jan Bouwman <jan.bouwman@ziggo.nl>

RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y wget binutils gcc psmisc tar libsqlite3-dev 
RUN apt-get clean && apt-get autoremove -y

RUN mkdir /home/apiui && cd /home/apiui
VOLUME /home/apiui
WORKDIR /home/apiui
COPY ./apiUi_Linux .
RUN chmod 755 ./apiUiServer
ENTRYPOINT ["./apiUiServer"]
