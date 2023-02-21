docker run -dit --rm --name=activemq `
	       --network dockernet `
           -p 61613:61613 `
           -p 8161:8161 `
	       rmohr/activemq
