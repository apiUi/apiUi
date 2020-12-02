# Parent Image

FROM debian:latest

ARG artifactlocation

ARG stubsgitrepolocation

#This should be the port that the stub is listening to

ENV PORT 7777

#Install some debugging/maintenance tools, openssl and git

RUN apt-get update && apt-get install -y procps vim curl openssl git

# Create Image using apiServer

COPY artifactlocation /opt
WORKDIR /opt/APIUI\ Server/

#Retrieve gitrepo with stubs

RUN git clone ${stubsgitrepolocation}

#Making sure the file can be run

RUN chmod +x ./apiServer

#Healthcheck, checks if it gets a responds when calling the port. (in the container)

HEALTHCHECK --interval=30s --timeout=40s --start-period=5s --retries=3 CMD curl -X GET http://localhost:${PORT}/apiUi/api/versionInfo

#exposing port so that docker knows

EXPOSE ${PORT}

#The command to actually run the stub server

CMD ["./apiServer", "../ServiceVirtualisation/Responders/RaboResponders.svpr", "--script=startUp"]



