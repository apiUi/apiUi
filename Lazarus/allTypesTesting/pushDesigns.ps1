echo "push project to apiuiserver..."
docker run -it --rm --name=apiuipushdesign `
	       --network dockernet `
	       -v ${PWD}:/proj `
	       apiui/apiuiserver `
		   --port=0 `
	       --project=/proj/allTypesResponder.svpr `
	       --context=dockernet `
           --script=pushDesignToDocker `
		   --terminate
