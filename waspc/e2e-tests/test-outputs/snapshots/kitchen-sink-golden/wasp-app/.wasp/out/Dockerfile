# syntax=docker/dockerfile:1
# NOTE: Why do we specify alpine version here?
#   Because if not, we had situations where it would use the different version
#   locally and on Github CI. This way we ensure exact version is used,
#   and also have control over updating it (instead of update surprising us).
FROM node:26.13.1-alpine3.23 AS node


# We split Dockerfile into base, server-builder and server-production.
# This way we have separate situations -> in server-builder we build all
# we need to run the server, and then in server-production we start fresh
# and just copy what we need from server-builder, avoiding intermediate
# artifacts and any settings / pollution we don't need in production
# but only for building.
#
# Docker reuses a layer only while every file copied before it is unchanged,
# so each stage copies files in the order they change least to most often.
# A code change then rebuilds only the steps that depend on the code.


FROM node AS base
RUN apk --no-cache -U upgrade # To ensure any potential security patches are applied.


# Todo: The 'server-builder' image stays on disk under <none>:<none> and is
# relatively large (~900 MB), should we remove it? Or is it useful for future
# builds?
FROM base AS server-builder
# Building the Docker image on Apple's Silicon Mac fails without python3 (the build
# throws `node-gyp` errors when it tries to compile native deps. Installing
# `python3` fixes the issue.
RUN apk add --no-cache python3 build-base libtool autoconf automake
WORKDIR /app
# A name with square brackets is a pattern, so Docker skips the file when the
# project doesn't have it.
COPY --link package.json package-lock.jso[n] ./
# Unreleased Wasp npm packages, which the project depends on through tarballs.
COPY --link dev-packages/ .wasp/out/dev-packages/
# Install scripts can read project files, so the full install runs again below,
# once the code is in place. This first install fills the package manager's
# cache, so the second one doesn't download the packages again.
RUN if [ -f package-lock.json ]; then npm ci --ignore-scripts; else npm install --ignore-scripts; fi
COPY --link db/schema.prisma .wasp/out/db/
# We mirror the same directory structure as the Wasp project.
COPY --link tsconfig*.json ./
# The project's tsconfig and the server's extend this one.
COPY --link tsconfig.paths.json .wasp/out/
COPY --link types .wasp/out/types
COPY --link sdk .wasp/out/sdk
COPY --link server .wasp/out/server
COPY --link src ./src
RUN if [ -f package-lock.json ]; then npm ci; else npm install; fi
# The generated code imports the packages that come with @wasp/wasp, and finds
# them through these links.
RUN node node_modules/@wasp/wasp/scripts/link-dependencies.js
WORKDIR /app/.wasp/out/server
# The Prisma CLI is among the packages linked above.
RUN ../node_modules/.bin/prisma generate --schema='../db/schema.prisma'
# Building the server should come after Prisma generation. npm only runs the
# script here, whatever the project's package manager is.
RUN npm run bundle && node prepareProduction.ts


# TODO: Use pm2?
# TODO: Use non-root user (node).
FROM base AS server-production
ENV NODE_ENV=production
WORKDIR /app/.wasp/out/server
# `production/` already contains every package the server needs, so this
# stage installs nothing.
# Dependencies and the bundle are separate layers, so a code change only ships
# the small bundle layer.
COPY --link --from=server-builder /app/.wasp/out/server/production/node_modules ./node_modules
COPY --link server/package.json server/loadDotEnv.cjs ./
COPY --link db/ /app/.wasp/out/db/
COPY --link --from=server-builder /app/.wasp/out/server/production/bundle ./bundle
ENTRYPOINT ["npm", "run", "start-production"]


# Any user-defined Dockerfile contents will be appended below.
## HELLO!

RUN echo "This will go at the end of the Dockerfile."

