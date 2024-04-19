#!/bin/bash
env_server_path="/workspaces/wasp/examples/tutorials/TodoApp/.env.server"
env_client_path="/workspaces/wasp/examples/tutorials/TodoApp/.env.client"

# Construct the base URL from the CODESPACE_NAME environment variable
base_url="https://${CODESPACE_NAME}"

# Set forwarded URLs for different services
wasp_web_client_url="${base_url}-3000.app.github.dev/"
react_app_api_url="${base_url}-3001.app.github.dev/"

# Write to .env.server
echo "WASP_WEB_CLIENT_URL=${wasp_web_client_url}" > "$env_server_path"

# Write to .env.client
echo "REACT_APP_API_URL=${react_app_api_url}" > "$env_client_path"


echo "Environment variables set:"
echo ".env.server: WASP_WEB_CLIENT_URL=${wasp_web_client_url}"
echo ".env.client: REACT_APP_API_URL=${react_app_api_url}"
