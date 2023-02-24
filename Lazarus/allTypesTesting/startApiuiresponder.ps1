echo "starting apiuiresponder..."
powershell docker run -dit --rm --name=apiuiresponder `
  --network dockernet `
  -p 7777:7777 `
  apiui/apiuiserver `
  --context=dockernet `
  --port=7777
