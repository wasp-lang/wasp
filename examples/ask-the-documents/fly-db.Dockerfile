# syntax=docker/dockerfile:1
# check=skip=FromPlatformFlagConstDisallowed (Fly.io only has amd64 machines)

# This image is published as ghcr.io/wasp-lang/ask-the-documents-fly-db
# Please update the public image if you change this file.

FROM --platform=linux/amd64 flyio/postgres-flex:17

RUN <<EOF
apt-get update -y
apt-get install -y postgresql-$PG_MAJOR_VERSION-pgvector
apt-get clean -y
EOF

# Tell GitHub to link the image to the repo
LABEL org.opencontainers.image.source=https://github.com/wasp-lang/wasp
