# NOTE: Why do we specify alpine version here?
#   Because if not, we had situations where it would use the different version
#   locally and on Github CI. This way we ensure exact version is used,
#   and also have control over updating it (instead of update surprising us).
FROM node:18 AS node


# We split Dockerfile into base, server-builder and server-production.
# This way we have separate situations -> in server-builder we build all
# we need to run the server, and then in server-production we start fresh
# and just copy what we need from server-builder, avoiding intermediate
# artifacts and any settings / pollution we don't need in production
# but only for building.


FROM node AS base
RUN apt update && apt upgrade -y # To ensure any potential security patches are applied.
# TODO: Remove line below (installation of openssl 1.1) once Prisma adds support for
#   openssl 3 on alpine. Alpine >= 3.17 has openssl 3 as default.
#   Relevant GH issue: https://github.com/wasp-lang/wasp/issues/877
# RUN apt install openssl1.1-compat


# Any user-defined Dockerfile contents will be appended below.
FROM base AS server-builder
WORKDIR /app
COPY server/ ./server/
# Install npm packages, resulting in node_modules/.
RUN cd server && npm install
COPY db/schema.prisma ./db/
RUN cd server && PRISMA_CLIENT_OUTPUT_DIR=../server/node_modules/.prisma/client/ npx prisma generate --schema='../db/schema.prisma'
# Building the server should come after Prisma generation.
RUN cd server && npm run build

FROM base AS server-production
RUN curl -sSL https://get.wasp-lang.dev/installer.sh | sh -s -- -v 0.11.1-wasp-ai-9
ENV PATH "$PATH:/root/.local/bin"
ENV NODE_ENV production
WORKDIR /app
COPY --from=server-builder /app/server/node_modules ./server/node_modules
COPY --from=server-builder /app/server/dist ./server/dist
COPY --from=server-builder /app/server/package*.json ./server/
COPY --from=server-builder /app/server/scripts ./server/scripts
COPY db/ ./db/
EXPOSE ${PORT}
WORKDIR /app/server
# RUN cat /root/.local/bin/wasp
# RUN wasp version
ENTRYPOINT ["npm", "run", "start-production"]
