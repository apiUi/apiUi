docker network create --driver bridge dockernet

docker run -dit --rm --name=activemq `
	       --network dockernet `
           -p 61613:61613 `
           -p 8161:8161 `
	       rmohr/activemq
docker run -dit --rm --name wiremock `
	       --network dockernet `
           -v ${PWD}/wiremock:/home/wiremock `
		   -p 7775:7775 `
		   wiremock/wiremock `
		   --port 7775 `
		   --verbose
docker run -dit --rm --name=apiuiresponder `
	       --network dockernet `
		   -p 7777:7777 `
	       -v ${PWD}:/proj `
	       apiui/apiuiserver `
	       --context=dockernet `
	       --project=/proj/allTypesResponder.svpr
