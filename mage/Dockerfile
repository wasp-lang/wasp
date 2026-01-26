# For Mage we are using the Debian Node.js image
FROM node:22 AS node

# We split Dockerfile into base, server-builder and server-production.
# This way we have separate situations -> in server-builder we build all
# we need to run the server, and then in server-production we start fresh
# and just copy what we need from server-builder, avoiding intermediate
# artifacts and any settings / pollution we don't need in production
# but only for building.

FROM node AS base
RUN apt update && apt upgrade -y # To ensure any potential security patches are applied.

# Todo: The 'server-builder' image stays on disk under <none>:<none> and is
# relatively large (~900 MB), should we remove it? Or is it useful for future
# builds?
FROM base AS server-builder
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
# Install npm packages, resulting in node_modules/.
RUN npm install && cd .wasp/out/server && npm install
COPY db/schema.prisma .wasp/out/db/
RUN cd .wasp/out/server && npx prisma generate --schema='../db/schema.prisma'
# Building the server should come after Prisma generation.
RUN cd .wasp/out/server && npm run bundle

# Depending on `npm`'s dependency resolution, this folder may or may not exist.
# We ensure they are created so that the COPY instruction later does not fail.
RUN mkdir -p /app/.wasp/out/server/node_modules

# TODO: Use pm2?
# TODO: Use non-root user (node).
FROM base AS server-production
RUN npm i -g @wasp.sh/wasp-cli@0.21.0
ENV NODE_ENV production
WORKDIR /app
# Copying the top level 'node_modules' because it contains the Prisma packages
# necessary for migrating the database.
COPY --from=server-builder /app/node_modules ./node_modules
# Copying 'server/node_modules' because we require dotenv package to
# load environment variables
# TODO: replace dotenv with native Node.js environment variable loading
COPY --from=server-builder /app/.wasp/out/server/node_modules .wasp/out/server/node_modules
COPY --from=server-builder /app/.wasp/out/server/bundle .wasp/out/server/bundle
COPY --from=server-builder /app/.wasp/out/server/package*.json .wasp/out/server/
COPY db/ .wasp/out/db/
EXPOSE ${PORT}
WORKDIR /app/.wasp/out/server
ENTRYPOINT ["npm", "run", "start-production"]
