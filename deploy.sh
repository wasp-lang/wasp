#!/bin/bash

set -euo pipefail

wasp build

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
cd build

echo -e "\n\033[1;33mDeploying client to Railway...\033[0m\n"
railway up --service client --detach
