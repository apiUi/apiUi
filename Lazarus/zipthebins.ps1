$folder = "./apiUi-Windows"
$zip = "./apiUi-Windows.zip"
if (test-path $folder) {remove-item -path $folder -recurse -force}
new-item -ItemType directory -Path $folder
Copy-Item -path .\apiUi\Config\ -Destination $folder -Recurse
Copy-Item -path .\apiUi\apiUi.exe -Destination $folder
Copy-Item -path .\apiUi\apiUi.bat -Destination $folder
Copy-Item -path .\apiUi\apiUiIni.xml -Destination $folder
Copy-Item -path .\apiUi\*.dll -Destination $folder
Copy-Item -path .\apiServer\apiServer.exe -Destination $folder
Copy-Item -path .\apiServer\apiServerIni.xml -Destination $folder
Compress-Archive -path $folder -DestinationPath $zip -Force
$folder = "./apiUi-Linux"
$zip = "./apiUi-Linux.zip"
if (test-path $folder) {remove-item -path $folder -recurse -force}
new-item -ItemType directory -Path $folder
Copy-Item -path .\apiUi\Config\ -Destination $folder -Recurse
Copy-Item -path .\apiUi\apiUi -Destination $folder
Copy-Item -path .\apiUi\apiUiIni.xml -Destination $folder
Copy-Item -path .\apiServer\apiServer -Destination $folder
Copy-Item -path .\apiServer\apiServerIni.xml -Destination $folder
Compress-Archive -path $folder -DestinationPath $zip -Force
