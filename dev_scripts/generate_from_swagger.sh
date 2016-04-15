
for L in 'python' 'java' 'html' 'haskell-servant' 'go' 'javascript' 'php' 'ruby' 'dynamic-html'
do
  OUT_DIR=../robot_examples/$L/rest_api
  rm -r -f $OUT_DIR
  mkdir -p $OUT_DIR
  java -jar swagger-codegen-cli-2.1.6.jar generate -i ../doc/rest_api.yaml -l $L -o $OUT_DIR
done

# The Python client needs this
touch ../robot_examples/python/rest_api/__init__.py

# Generate the JSON API for the server.
OUT_DIR=../server/rest_api
rm -r -f $OUT_DIR
mkdir -p $OUT_DIR
cp -r ../robot_examples/python/rest_api/swagger_client/* $OUT_DIR
