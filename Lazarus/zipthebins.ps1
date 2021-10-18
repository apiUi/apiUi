$folder = "./apiUi_Windows"
$zip = "./apiUi_Windows.zip"
if (test-path $folder) {remove-item -path $folder -recurse -force}
new-item -ItemType directory -Path $folder
Copy-Item -path .\apiUi\Config\ -Destination $folder -Recurse
Copy-Item -path .\apiUi\apiUi.exe -Destination $folder
Copy-Item -path .\apiUi\apiUi.bat -Destination $folder
Copy-Item -path .\apiUi\apiUiIni.xml -Destination $folder
Copy-Item -path .\apiUi\*.dll -Destination $folder
Copy-Item -path .\apiUiServer\apiUiServer.exe -Destination $folder
Copy-Item -path .\apiUiServer\apiUiServerIni.xml -Destination $folder
Compress-Archive -path $folder -DestinationPath $zip -Force
$folder = "./apiUi_Linux"
$zip = "./apiUi_Linux.zip"
if (test-path $folder) {remove-item -path $folder -recurse -force}
new-item -ItemType directory -Path $folder
Copy-Item -path .\apiUi\Config\ -Destination $folder -Recurse
Copy-Item -path .\apiUi\apiUi -Destination $folder
Copy-Item -path .\apiUi\apiUiIni.xml -Destination $folder
Copy-Item -path .\apiUiServer\apiUiServer -Destination $folder
Copy-Item -path .\apiUiServer\apiUiServerIni.xml -Destination $folder
Copy-Item -path .\apiUiServer\libssl.so -Destination $folder
Copy-Item -path .\apiUiServer\libcrypto.so -Destination $folder
Compress-Archive -path $folder -DestinationPath $zip -Force
