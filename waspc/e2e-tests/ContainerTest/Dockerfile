FROM node:22-alpine3.21 AS runtime
# We use curl and bash syntax in our commands.
RUN apk update && apk add curl bash
SHELL ["/bin/bash", "-c"] 
# Ensure we have `.local/bin` in our path.
RUN mkdir -p /root/.local/bin/
ENV PATH=/root/.local/bin:$PATH
# Copy the `wasp-cli` binary from host.
ARG WASP_CLI_PATH
COPY ./${WASP_CLI_PATH} /root/.local/bin/wasp-cli
# Make the binary executable
RUN chmod +x /root/.local/bin/wasp-cli
# Ensure we have development version of Wasp installed properly.
RUN wasp-cli version
