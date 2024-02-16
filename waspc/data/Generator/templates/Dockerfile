{{={= =}=}}
# NOTE: Why do we specify alpine version here?
#   Because if not, we had situations where it would use the different version
#   locally and on Github CI. This way we ensure exact version is used,
#   and also have control over updating it (instead of update surprising us).
# todo: issue update alpine na 4.18, nema verzije na dockeru. I sto cemo s node verzijom, mozda na 20? 
FROM node:{= nodeVersion =}-alpine3.17 AS node


# We split Dockerfile into base, server-builder and server-production.
# This way we have separate situations -> in server-builder we build all
# we need to run the server, and then in server-production we start fresh
# and just copy what we need from server-builder, avoiding intermediate
# artifacts and any settings / pollution we don't need in production
# but only for building.


FROM node AS base
RUN apk --no-cache -U upgrade # To ensure any potential security patches are applied.


# Todo this image stays on disk under <none>:<none>, should we remove it? Or is it useful for future builds?
FROM base AS server-builder
RUN apk add --no-cache python3 build-base libtool autoconf automake
WORKDIR /app
COPY src ./src
COPY package.json .
COPY package-lock.json .
COPY .wasp/build/server .wasp/build/server
COPY .wasp/out/sdk .wasp/out/sdk
# Install npm packages, resulting in node_modules/.
RUN npm install && cd .wasp/build/server && npm install
{=# usingPrisma =}
COPY .wasp/build/db/schema.prisma .wasp/build/db/
RUN cd .wasp/build/server && npx prisma generate --schema='{= dbSchemaFileFromServerDir =}'
{=/ usingPrisma =}
# Building the server should come after Prisma generation.
RUN cd .wasp/build/server && npm run bundle


# TODO: Use pm2?
# TODO: Use non-root user (node).
FROM base AS server-production
# In case they want to use python3 in their app.
RUN apk add --no-cache python3
ENV NODE_ENV production
WORKDIR /app
# Copying the top level 'node_modules' because it contains the Prisma packages
# necessary for migrating the database.
COPY --from=server-builder /app/node_modules ./node_modules
# Copying the SDK because 'validate-env.mjs' executes independent of the bundle
# and references the 'wasp' package.
COPY --from=server-builder /app/.wasp/out/sdk .wasp/out/sdk
# Copying 'server/node_modules' because 'validate-env.mjs' executes independent
# of the bundle and references the dotenv package.
COPY --from=server-builder /app/.wasp/build/server/node_modules .wasp/build/server/node_modules
COPY --from=server-builder /app/.wasp/build/server/bundle .wasp/build/server/bundle
COPY --from=server-builder /app/.wasp/build/server/package*.json .wasp/build/server/
COPY --from=server-builder /app/.wasp/build/server/scripts .wasp/build/server/scripts
COPY .wasp/build/db/ .wasp/build/db/
EXPOSE ${PORT}
WORKDIR /app/.wasp/build/server
ENTRYPOINT ["npm", "run", "start-production"]


# Any user-defined Dockerfile contents will be appended below.
{=& userDockerfile =}
