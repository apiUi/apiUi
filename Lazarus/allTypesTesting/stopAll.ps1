echo "stop all..."
docker container stop apiuiresponder activemq wiremock
docker network rm dockernet
docker volume prune -f
