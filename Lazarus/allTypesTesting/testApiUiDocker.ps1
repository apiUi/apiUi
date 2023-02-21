echo "starting all..."
powershell .\startAll.ps1

powershell .\pushDesignToApiUiServer.ps1

powershell .\runRequestor.ps1

powershell .\saveSnapshot.ps1

echo "stop all...."
powershell .\stopAll.ps1

powershell .\startApiUi.ps1
