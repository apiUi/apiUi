FROM debian:latest
MAINTAINER Jan Bouwman <jan.bouwman@ziggo.nl>

# Install required tools for Lazarus
RUN apt-get -y update && \
    apt-get -y install git sudo binutils build-essential zip unzip wget libusb-dev libsane-dev libdbus-1-dev sqlite3 libsqlite3-dev postgresql-client binutils libgtk2.0-0 libgtk2.0-dev psmisc subversion
RUN apt-get clean && apt-get autoremove -y

# Install Lazarus 2.0.10 & the free pascal compiler 3.2.0
RUN echo "Downloading and installing Debians" && \
    cd /tmp && \
    wget "https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%202.0.10/fpc-laz_3.2.0-1_amd64.deb" -O fpc.deb && \
    wget "https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%202.0.10/fpc-src_3.2.0-1_amd64.deb" -O fpc-src.deb &&\
    wget "https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%202.0.10/lazarus-project_2.0.10-0_amd64.deb"  -O lazarus.deb
RUN ls /tmp/* && \
    dpkg -i --force-depends /tmp/fpc-src.deb && \
    dpkg -i --force-depends /tmp/fpc.deb && \
    dpkg -i --force-depends /tmp/lazarus.deb

# install APIUI dependencies:
RUN mkdir /install && cd /install && \
    wget "https://packages.lazarus-ide.org/Indy10.zip" && unzip Indy10.zip && lazbuild Indy10/indylaz.lpk && rm Indy10.zip &&\
    wget "https://packages.lazarus-ide.org/Abbrevia.zip" && unzip Abbrevia.zip && lazbuild Abbrevia/packages/Lazarus/abbrevia.lpk && rm Abbrevia.zip && \
    wget "https://packages.lazarus-ide.org/LCLExtensions.zip" && unzip LCLExtensions.zip && lazbuild lclextensions/lclextensions_package.lpk && rm LCLExtensions.zip && \
    wget "https://packages.lazarus-ide.org/VirtualTreeViewV5.zip" && unzip VirtualTreeViewV5.zip && lazbuild VirtualTreeView\ V5/Source/virtualtreeview_package.lpk && rm VirtualTreeViewV5.zip && \
    wget "https://packages.lazarus-ide.org/HtmlViewer.zip" && unzip HtmlViewer.zip && lazbuild HtmlViewer/package/FrameViewer09.lpk  && rm HtmlViewer.zip && \
    wget "https://github.com/mriscoc/fpc-markdown/archive/refs/tags/v1.1.1.zip" && unzip v1.1.1.zip && lazbuild fpc-markdown-1.1.1/fpc_markdown.lpk && rm v1.1.1.zip

WORKDIR /app
ADD Lazarus .
RUN echo "Compiling apiUiServer" && \
    cd ./apiUiServer && \
	lazbuild apiUiServer.lpi && \
	cd ../apiUi && \
	lazbuild apiUi.lpi && \
	cd ..




