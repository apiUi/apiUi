echo "testing..."
powershell .\startAll.ps1
powershell .\pushDesigns.ps1
powershell .\runRequestor.ps1
powershell .\stopAll.ps1
powershell .\simul8rtests.ps1
powershell .\startApiUi.ps1
