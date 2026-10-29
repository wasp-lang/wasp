{{={= =}=}}
# syntax=docker/dockerfile:1
# NOTE: Why do we specify alpine version here?
#   Because if not, we had situations where it would use the different version
#   locally and on Github CI. This way we ensure exact version is used,
#   and also have control over updating it (instead of update surprising us).
FROM node:{= nodeVersion =}-alpine3.23 AS node


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
COPY --link package.json package-lock.json ./
COPY --link server/package.json .wasp/out/server/
COPY --link sdk/wasp/package.json .wasp/out/sdk/wasp/
COPY --link libs .wasp/out/libs
RUN npm install
WORKDIR /app/.wasp/out/server
{=# usingPrisma =}
# Generating the Prisma client depends only on the schema, so it comes before
# the code and survives code changes.
COPY --link db/schema.prisma /app/.wasp/out/db/
RUN npx prisma generate --schema='{= dbSchemaFileFromServerDir =}'
{=/ usingPrisma =}
# We mirror the same directory structure as the Wasp project.
COPY --link tsconfig*.json /app/
COPY --link types /app/.wasp/out/types
COPY --link sdk /app/.wasp/out/sdk
COPY --link server ./
COPY --link src /app/src
# Building the server should come after Prisma generation.
RUN npm run bundle && node prepareProduction.ts


# TODO: Use pm2?
# TODO: Use non-root user (node).
FROM base AS server-production
ENV NODE_ENV=production
WORKDIR /app/.wasp/out/server
# `production/` already contains every package the server needs, so this
# stage needs no `npm install`.
# Dependencies and the bundle are separate layers, so a code change only ships
# the small bundle layer.
COPY --link --from=server-builder /app/.wasp/out/server/production/node_modules ./node_modules
COPY --link server/package.json ./
COPY --link db/ /app/.wasp/out/db/
COPY --link --from=server-builder /app/.wasp/out/server/production/bundle ./bundle
ENTRYPOINT ["npm", "run", "start-production"]


# Any user-defined Dockerfile contents will be appended below.
{=& userDockerfile =}
