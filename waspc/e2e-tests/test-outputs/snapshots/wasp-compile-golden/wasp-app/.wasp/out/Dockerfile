# NOTE: Why do we specify alpine version here?
#   Because if not, we had situations where it would use the different version
#   locally and on Github CI. This way we ensure exact version is used,
#   and also have control over updating it (instead of update surprising us).
FROM node:24.14.1-alpine3.23 AS node


# We split the Dockerfile into base, builder and production. This way the
# builder has everything needed to build the app, and production starts fresh
# and only copies what it takes to run it, leaving the intermediate artifacts
# and the build-only tooling behind.


FROM node AS base
RUN apk --no-cache -U upgrade # To ensure any potential security patches are applied.
# Alpine 3.21+ no longer includes openssl as a transitive dependency of the
# Node.js image. Prisma's native engine needs libssl at both build and runtime.
# TODO: Revisit once we upgrade to Prisma 6 (https://github.com/wasp-lang/wasp/issues/2504).
RUN apk add --no-cache openssl


# Todo: The 'builder' image stays on disk under <none>:<none> and is
# relatively large (~900 MB), should we remove it? Or is it useful for future
# builds?
FROM base AS builder
# Building the Docker image on Apple's Silicon Mac fails without python3 (the build
# throws `node-gyp` errors when it tries to compile native deps. Installing
# `python3` fixes the issue.
RUN apk add --no-cache python3 build-base libtool autoconf automake
WORKDIR /app
# The app is built exactly like it is in development: with Vite, from the
# project's root directory. So we mirror that directory structure here, with
# the user's own files (their source, their Vite config, ...) next to the ones
# Wasp generated for them.
COPY src ./src
COPY public ./public
COPY package.json .
COPY package-lock.json .
COPY tsconfig*.json .
COPY vite.config.* .
COPY server .wasp/out/server
COPY sdk .wasp/out/sdk
COPY types .wasp/out/types
COPY libs .wasp/out/libs
# Install npm packages, resulting in node_modules/.
RUN npm install && cd .wasp/out/server && npm install
# Environment variables prefixed with `REACT_APP_` end up inside the app's
# pages and assets, so the ones the app needs have to be here, where they are
# built. This build argument takes them as shell assignments, one per line
# (`REACT_APP_EXAMPLE='value'`).
ARG WASP_CLIENT_ENV=
RUN set -a && eval "$WASP_CLIENT_ENV" && set +a && npx vite build


# TODO: Use pm2?
# TODO: Use non-root user (node).
FROM base AS production
# In case they want to use python3 in their app.
RUN apk add --no-cache python3
ENV NODE_ENV=production
WORKDIR /app
# The built app: one server that serves both the app's pages and its API. It
# carries everything it needs (the Prisma engine included) with it.
COPY --from=builder /app/.wasp/out/web-app .wasp/out/web-app
# The generated server's package, which is where the app is started from (see
# the entrypoint below), and where its migrations are applied from.
COPY --from=builder /app/.wasp/out/server/package.json .wasp/out/server/
# The top level 'node_modules', because it contains the Prisma CLI the app
# migrates the database with when it starts.
# TODO: Only the Prisma CLI is needed here, not the whole dependency tree.
COPY --from=builder /app/node_modules ./node_modules
COPY db/ .wasp/out/db/
EXPOSE ${PORT}
WORKDIR /app/.wasp/out/server
ENTRYPOINT ["npm", "run", "start-production"]


# Any user-defined Dockerfile contents will be appended below.

