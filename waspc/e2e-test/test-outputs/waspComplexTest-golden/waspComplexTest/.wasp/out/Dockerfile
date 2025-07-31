# NOTE: Why do we specify alpine version here?
#   Because if not, we had situations where it would use the different version
#   locally and on Github CI. This way we ensure exact version is used,
#   and also have control over updating it (instead of update surprising us).
FROM node:22.12.0-alpine3.20 AS node


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
# Since the framwork code in /.wasp/build/server imports the user code in /src
# using relative imports, we must mirror the same directory structure in the
# Docker image.
COPY src ./src
COPY package.json .
COPY package-lock.json .
COPY tsconfig*.json .
COPY server .wasp/build/server
COPY sdk .wasp/out/sdk
# Install npm packages, resulting in node_modules/.
RUN npm install && cd .wasp/build/server && npm install
COPY db/schema.prisma .wasp/build/db/
RUN cd .wasp/build/server && npx prisma generate --schema='../db/schema.prisma'
# Building the server should come after Prisma generation.
RUN cd .wasp/build/server && npm run bundle


# TODO: Use pm2?
# TODO: Use non-root user (node).
FROM base AS server-production
# In case they want to use python3 in their app.
RUN apk add --no-cache python3
ENV NODE_ENV=production
WORKDIR /app
# Copying the top level 'node_modules' because it contains the Prisma packages
# necessary for migrating the database.
COPY --from=server-builder /app/node_modules ./node_modules
# Copying 'server/node_modules' because we require dotenv package to
# load environment variables
# TODO: replace dotenv with native Node.js environment variable loading
COPY --from=server-builder /app/.wasp/build/server/node_modules .wasp/build/server/node_modules
COPY --from=server-builder /app/.wasp/build/server/bundle .wasp/build/server/bundle
COPY --from=server-builder /app/.wasp/build/server/package*.json .wasp/build/server/
COPY db/ .wasp/build/db/
EXPOSE ${PORT}
WORKDIR /app/.wasp/build/server
ENTRYPOINT ["npm", "run", "start-production"]


# Any user-defined Dockerfile contents will be appended below.

