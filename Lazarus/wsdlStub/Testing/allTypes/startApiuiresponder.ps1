docker run -dit --rm --name=apiuiresponder `
  --network dockernet `
  -p 7777:7777 `
  -v ${PWD}:/proj `
  apiui/apiuiserver `
  --context=dockernet `
  --project=/proj/remoteserver/remote.svpr
