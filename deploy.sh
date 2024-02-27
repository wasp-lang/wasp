#!/bin/bash

set -euo pipefail

wasp-cli build

# Deploy the server
cd .wasp/build


echo -e "\n\033[1;33mDeploying server to Railway...\033[0m\n"


railway up --service server --detach

# Deploy the client
if [ -z "$BACKEND_URL" ]
then
  echo "BACKEND_URL is not set"
  exit 1
fi

cd web-app
npm install && REACT_APP_API_URL=$BACKEND_URL npm run build
cp -r build dist

dockerfile_contents=$(cat <<EOF
FROM pierrezemb/gostatic
CMD [ "-fallback", "index.html" ]
COPY ./dist/ /srv/http/
EOF
)

dockerignore_contents=$(cat <<EOF
node_modules/
EOF
)

echo "$dockerfile_contents" > Dockerfile
echo "$dockerignore_contents" > .dockerignore

echo -e "\n\033[1;33mDeploying client to Railway...\033[0m\n"
railway up --service client --detach
