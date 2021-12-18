docker build --tag lazapiuibuilder .
docker run --name lazapiuibuilder lazapiuibuilder
docker cp lazapiuibuilder:app/apiUi/apiUi ./Lazarus/apiUi/
docker cp lazapiuibuilder:app/apiUiServer/apiUiServer ./Lazarus/apiUiServer/
docker rm lazapiuibuilder
