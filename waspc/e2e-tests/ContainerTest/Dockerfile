FROM --platform=linux/amd64 debian:trixie-slim

# Minimal requirements to install the Wasp CLI: curl and node.
RUN apt-get update && apt-get install -y curl
# Ensure we get the latest Node LTS version.
RUN curl -fsSL https://deb.nodesource.com/setup_lts.x | bash -
RUN apt-get install -y nodejs

# Ensure we have `.local/bin` in our path. It's what Wasp CLI expects.
ENV PATH=/root/.local/bin:$PATH

# Since we use `wasp-cli` exclusively in `waspc` e2e tests.
RUN mkdir -p /root/.local/bin && ln -s /root/.local/bin/wasp-cli /root/.local/bin/wasp
