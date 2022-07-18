{{={= =}=}}
FROM node:{= nodeMajorVersion =}-alpine AS node


FROM node AS base
RUN apk --no-cache -U upgrade # To ensure any potential security patches are applied.


FROM base AS server-builder
# Install packages needed to build native npm packages.
RUN apk add --no-cache build-base libtool autoconf automake python3
WORKDIR /app
# Install npm packages, resulting in node_modules/.
COPY server/patches ./server/patches
COPY server/package*.json ./server/
RUN cd server && npm install
{=# usingPrisma =}
COPY db/schema.prisma ./db/
RUN cd server && npx prisma generate --schema=../db/schema.prisma
{=/ usingPrisma =}


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
