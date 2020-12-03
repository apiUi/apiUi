#!/bin/bash
#Adding the path to the private key for the Stubs Repo
ssh-add /etc/secret-volume/id_rsa
#Cloning the Stubs repo
git clone $GIT_URL .
#Starting the APIUI Server with reference to the stubs location
/opt/APIUI\ Server/apiServer $(find . -name "*Responders.svpr" | head -n 1) --script=startUp