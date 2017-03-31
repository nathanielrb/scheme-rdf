sudo docker run --name virtuoso-eurostat \
     -p 8890:8890 -p 1111:1111 \
     -e DBA_PASSWORD=admin \
     -e SPARQL_UPDATE=true \
     -e DEFAULT_GRAPH=http://www.tenforce.com/eurostat \
     -v /home/nathaniel/data/virtuoso/eurostat/data \
     -d tenforce/virtuoso
