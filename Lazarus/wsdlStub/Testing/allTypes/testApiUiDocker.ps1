powershell .\startServers.ps1

docker run -dit --rm --name=apiuirequestor `
	       --network dockernet `
	       -p 7776:7776 `
	       -v ${PWD}:/proj `
	       apiui/apiuiserver `
	       --project=/proj/allTypesRequestor.svpr `
	       --context=dockernet `
           --script=runTests

<#
$Params = @{
    Method = "Post"
    Uri = "http://localhost:7776/apiUi/api/snapshot/checkregression"
    Body = @{name = "testRun"} | ConvertTo-Json
    ContentType = "application/json"
}
Invoke-RestMethod @Params
#>
docker container stop apiuirequestor

powershell .\stopServers.ps1

