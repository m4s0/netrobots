

for L in 'python' 'java' 'html' 'haskell-servant' 'go' 'javascript' 'php' 'ruby' 'dynamic-html'
do
  OUT_DIR=../client/generated_rest_api/$L
  mkdir -p $OUT_DIR
  java -jar swagger-codegen-cli-2.1.6.jar generate -i ../doc/rest_api.yaml -l $L -o $OUT_DIR
done

# Generate the JSON API for the server.
OUT_DIR=../server/rest_api
rm -r -f $OUT_DIR
cp -r ../client/generated_rest_api/python/swagger_client $OUT_DIR
