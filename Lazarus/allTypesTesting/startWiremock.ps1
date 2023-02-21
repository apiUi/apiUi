docker run -dit --rm --name wiremock `
	       --network dockernet `
           -v ${PWD}/wiremock:/home/wiremock `
		   -p 7775:7775 `
		   wiremock/wiremock `
		   --port 7775 `
		   --verbose `
		   --local-response-templating
