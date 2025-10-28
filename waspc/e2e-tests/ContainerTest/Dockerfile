FROM debian:bookworm AS runtime
# We use curl in our commands.
# Node is required to run Wasp CLI.
RUN apt-get update -y
RUN apt-get install -y curl
RUN curl -fsSL https://deb.nodesource.com/setup_lts.x | bash -
RUN apt-get install -y nodejs
# Ensure we have `.local/bin` in our path.
RUN mkdir -p /root/.local/bin/
ENV PATH=/root/.local/bin:$PATH
# Copy the `wasp-cli` binary from host.
ARG WASP_CLI_PATH
COPY ./${WASP_CLI_PATH} /root/.local/bin/wasp-cli
# To enable testing of default behavior.
# e.g. uninstall removes the binary
RUN ln -s /root/.local/bin/wasp-cli /root/.local/bin/wasp
# Make the binary executable.
RUN chmod +x /root/.local/bin/wasp-cli
