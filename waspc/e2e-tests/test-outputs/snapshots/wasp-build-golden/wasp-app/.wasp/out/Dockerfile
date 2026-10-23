# NOTE: Why do we specify alpine version here?
#   Because if not, we had situations where it would use the different version
#   locally and on Github CI. This way we ensure exact version is used,
#   and also have control over updating it (instead of update surprising us).
FROM node:24.14.1-alpine3.23 AS node


# We split Dockerfile into base, server-builder and server-production.
# This way we have separate situations -> in server-builder we build all
# we need to run the server, and then in server-production we start fresh
# and just copy what we need from server-builder, avoiding intermediate
# artifacts and any settings / pollution we don't need in production
# but only for building.


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
# Since the framwork code in /.wasp/out/server imports the user code in /src
# using relative imports, we must mirror the same directory structure in the
# Docker image.
COPY src ./src
COPY package.json .
COPY package-lock.json .
COPY tsconfig*.json .
COPY server .wasp/out/server
COPY sdk .wasp/out/sdk
COPY types .wasp/out/types
COPY libs .wasp/out/libs
# Install npm packages, resulting in node_modules/.
RUN npm install && cd .wasp/out/server && npm install
# Building the server should come after Prisma generation.
RUN cd .wasp/out/server && npm run bundle

# Copy the bundle and only the files it needs at runtime into
# `.wasp/out/server/production`. See `traceProductionDeps.js`.
RUN cd .wasp/out/server && node traceProductionDeps.js


# TODO: Use pm2?
# TODO: Use non-root user (node).
FROM base AS server-production
# In case they want to use python3 in their app.
RUN apk add --no-cache python3
ENV NODE_ENV=production
WORKDIR /app
# The traced output holds the bundle, a minimal `node_modules` and the
# production `package.json`. Nothing gets installed in this stage.
COPY --from=server-builder /app/.wasp/out/server/production .wasp/out/server
COPY db/ .wasp/out/db/
EXPOSE ${PORT}
WORKDIR /app/.wasp/out/server
ENTRYPOINT ["npm", "run", "start-production"]


# Any user-defined Dockerfile contents will be appended below.

