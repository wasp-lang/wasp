FROM mcr.microsoft.com/devcontainers/javascript-node:0-20-bullseye

RUN curl -sSL https://get.wasp-lang.dev/installer.sh | sh

# CODESPACE_NAME is an env variable available in Github Codepsaces
# https://docs.github.com/en/codespaces/developing-in-codespaces/default-environment-variables-for-your-codespace
RUN printf "export WASP_TELEMETRY_USER_ID=\"\$(echo -n \$GITHUB_USER | sha256sum | cut -d' ' -f1)\"\n" >> $HOME/.bashrc
RUN printf 'export WASP_WEB_CLIENT_URL=https://$CODESPACE_NAME-3000.preview.app.github.dev\n' >> $HOME/.bashrc
RUN printf 'export REACT_APP_API_URL=https://$CODESPACE_NAME-3001.preview.app.github.dev\n' >> $HOME/.bashrc