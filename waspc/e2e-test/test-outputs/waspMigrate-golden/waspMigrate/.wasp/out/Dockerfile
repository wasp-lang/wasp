# NOTE: Why do we specify alpine version here?
#   Because if not, we had situations where it would use the different version
#   locally and on Github CI. This way we ensure exact version is used,
#   and also have control over updating it (instead of update surprising us).
FROM node:18-alpine3.17 AS node


FROM node AS base
RUN apk --no-cache -U upgrade # To ensure any potential security patches are applied.


FROM base AS server-builder
RUN apk add --no-cache build-base libtool autoconf automake python3
# TODO: Remove line below (installation of openssl 1.1) once Prisma adds support for
#   openssl 3 on alpine. Alpine >= 3.17 has openssl 3 as default.
#   Relevant GH issue: https://github.com/wasp-lang/wasp/issues/877
RUN apk add --no-cache openssl1.1-compat
WORKDIR /app
# Install npm packages, resulting in node_modules/.
COPY server/package*.json ./server/
RUN cd server && npm install
COPY db/schema.prisma ./db/
RUN cd server && npx prisma generate --schema=../db/schema.prisma


# TODO: Use pm2?
# TODO: Use non-root user (node).
FROM base AS server-production
ENV NODE_ENV production
WORKDIR /app
COPY --from=server-builder /app/server/node_modules ./server/node_modules
COPY server/ ./server/
COPY db/ ./db/
EXPOSE ${PORT}
WORKDIR /app/server
ENTRYPOINT ["npm", "run", "start-production"]


# Any user-defined Dockerfile contents will be appended below.

