echo "run requestor"
docker run -dit --rm --name=apiuirequestor `
	       --network dockernet `
	       -v ${PWD}:/proj `
	       apiui/apiuiserver `
	       --project=/proj/allTypesRequestor.svpr `
	       --context=dockernet `
           --script=runTests `
		   --terminate `
		   --port=0
