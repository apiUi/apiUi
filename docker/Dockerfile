FROM debian:latest
MAINTAINER Jan Bouwman <jan.bouwman@ziggo.nl>

RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y wget binutils gcc psmisc tar libsqlite3-dev

RUN mkdir /install && cd /install && \
    wget "https://www.apiui.org/xpages/downloads/apiUi_Linux.tar.gz" && \
	tar -xvf ./apiUi_Linux.tar.gz  && \
	rm ./apiUi_Linux.tar.gz && \
	mv /install/apiUi_Linux ../apiui && \
	rmdir /install && \
	chmod 755 /apiui/apiUiServer

RUN apt-get clean && apt-get autoremove -y

WORKDIR /home/apiui

ENTRYPOINT ["/apiui/apiUiServer"]
