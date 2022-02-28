docker run -it --rm -v ${PWD}:/home/lazarus apiui/apiuilazbuild apiUiServer/apiUiServer.lpi

$folder = "./apiUi_Windows"
$zip = "./apiUi_Windows.zip"
if (test-path $folder) {remove-item -path $folder -recurse -force}
new-item -ItemType directory -Path $folder
Copy-Item -path .\apiUi\Config\ -Destination $folder -Recurse
Copy-Item -path .\apiUi\*.exe -Destination $folder
Copy-Item -path .\apiUiServer\*.exe -Destination $folder
Copy-Item -path .\apiUi\apiUi.bat -Destination $folder
Copy-Item -path .\apiUi\*.xml -Destination $folder
Copy-Item -path .\apiUiServer\*.xml -Destination $folder
Copy-Item -path .\apiUi\*.dll -Destination $folder
Compress-Archive -path $folder -DestinationPath $zip -Force
$folder = "./apiUi_Linux"
$zip = "./apiUi_Linux.tar.gz"
if (test-path $folder) {remove-item -path $folder -recurse -force}
new-item -ItemType directory -Path $folder
Copy-Item -path .\apiUi\Config\ -Destination $folder -Recurse
Copy-Item -path .\apiUi\apiUi -Destination $folder
Copy-Item -path .\apiUiServer\apiUiServer -Destination $folder
Copy-Item -path .\apiUi\apiUiIni.xml -Destination $folder
Copy-Item -path .\apiUiServer\apiUiServerIni.xml -Destination $folder
Copy-Item -path .\apiUi\*.xml -Destination $folder
Copy-Item -path .\apiUi\*.so -Destination $folder
tar -czf $zip .\apiUi_Linux

docker build --tag apiui/apiuiserver .

