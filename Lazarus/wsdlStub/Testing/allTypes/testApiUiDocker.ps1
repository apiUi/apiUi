echo "starting all..."
powershell .\startAll.ps1

echo "push project to apiuiserver..."
docker run -it --rm --name=apiuipushdesign `
	       --network dockernet `
	       -p 7776:7777 `
	       -v ${PWD}:/proj `
	       apiui/apiuiserver `
	       --project=/proj/allTypesResponder.svpr `
	       --context=dockernet `
           --script=pushDesignToDocker `
		   --terminate

echo "run requestor"
docker run -dit --rm --name=apiuirequestor `
	       --network dockernet `
	       -p 7776:7776 `
	       -v ${PWD}:/proj `
	       apiui/apiuiserver `
	       --project=/proj/allTypesRequestor.svpr `
	       --context=dockernet `
           --script=runTests

echo "retrieve regression"
<# #>
$Body = @{name = "testRun"}
$Params = @{
    Method = "Post"
    Uri = "http://localhost:7776/apiUi/api/snapshot/checkregression"
    Body = $Body | ConvertTo-Json
    ContentType = "application/json"
}
Invoke-RestMethod @Params
<# #>

echo "Sleep..."
Start-Sleep -Seconds 10

echo "stop requestor..."
docker container stop apiuirequestor

echo "stop all...."
powershell .\stopAll.ps1

