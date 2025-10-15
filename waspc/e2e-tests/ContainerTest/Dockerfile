FROM node:22-alpine3.21 AS runtime
# We use curl in our commands
RUN apk update && apk add curl
# Ensure we have `.local/bin` in our path.
ENV PATH=~/.local/bin:$PATH
# Ensure we have development version of Wasp installed (`wasp-cli`).
ARG WASP_CLI_PATH
COPY ${WASP_CLI_PATH} ~/.local/bin/wasp-cli
RUN wasp-cli version
