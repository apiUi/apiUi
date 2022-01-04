docker network create --driver bridge dockernet

docker run -dit --rm --name=activemq `
	   --network dockernet `
       -p 61613:61613 `
       -p 8161:8161 `
	   rmohr/activemq
docker run -dit --rm --name=apiuiresponder `
	   --network dockernet `
	   -v ${PWD}:/proj `
	   apiuiserver `
	   --context=dockernet `
	   --project=/proj/allTypesResponder.svpr

Start-Sleep -s 5	   
docker run -dit --rm --name=apiuirequestor `
	   --network dockernet `
	   -p 7776:7776 `
	   -v ${PWD}:/proj `
	   apiuiserver `
	   --project=/proj/allTypesRequestor.svpr `
	   --context=dockernet `
       --script=runTests
Start-Sleep -s 5	   
$Params = @{
    Method = "Post"
    Uri = "http://localhost:7776/apiUi/api/snapshot/checkregression"
    Body = @{name = "testRun"} | ConvertTo-Json
    ContentType = "application/json"
}
Invoke-RestMethod @Params
docker container stop apiuirequestor apiuiresponder activemq
docker network rm dockernet
