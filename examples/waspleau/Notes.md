# Observations

## 1) Loading hard coded data from query (but no entities) causes Prisma error

I was attempting to return hard coded JSON from a query without any entities defined, but ran into the following issue on `wasp start`:

```
Server: /Users/shayne/dev/wasp/examples/waspleau/.wasp/out/server/node_modules/.prisma/client/index.js:3
Server (stderr):     throw new Error(
Server (stderr):           ^
Server (stderr):
Server (stderr): Error: @prisma/client did not initialize yet. Please run "prisma generate" and try to import it again.
Server (stderr): In case this error is unexpected for you, please report it in https://github.com/prisma/prisma/issues
Server (stderr):     at new PrismaClient (/Users/shayne/dev/wasp/examples/waspleau/.wasp/out/server/node_modules/.prisma/client/index.js:3:11)
Server (stderr):     at createDbClient (file:///Users/shayne/dev/wasp/examples/waspleau/.wasp/out/server/src/dbClient.js:5:24)
Server (stderr):     at file:///Users/shayne/dev/wasp/examples/waspleau/.wasp/out/server/src/dbClient.js:11:18
Server (stderr):     at ModuleJob.run (internal/modules/esm/module_job.js:152:23)
Server (stderr):     at async Loader.import (internal/modules/esm/loader.js:166:24)
Server (stderr):     at async Object.loadESM (internal/process/esm_loader.js:68:5)
Server (stderr): [nodemon] app crashed - waiting for file changes before starting...
```

Therefore, I assumed I needed to migrate, however...

### Running `wasp db migrate-dev` without any entities defined

```
Database successfully set up!
Copying migrations folder from Wasp to Prisma project...
Done copying migrations folder.
Performing migration...
Prisma schema loaded from ../db/schema.prisma
Datasource "db": SQLite database "dev.db" at "file:./dev.db"

Already in sync, no schema change or pending migration was found.

Running generate... (Use --skip-generate to skip the generators)
Error:
You don't have any models defined in your schema.prisma, so nothing will be generated.
You can define a model like this:

model User {
  id    Int     @id @default(autoincrement())
  email String  @unique
  name  String?
}

More information in our documentation:
https://pris.ly/d/prisma-schema

Error: Migrate dev failed: Migrate (dev) failed with exit code: 1
```

The fix was to add a dummy migration just to get past the Prisma error.


| Bug captured here: https://github.com/wasp-lang/wasp/issues/398 |
| --------------------------------------------------------------- |

## 2) Axios version pinning

If you add `axios` as a dependency with a different version (like latest), you get the following error since Wasp uses it too:

```
wasp: Error: Dependency conflict for user npm dependency (axios, ^0.21.1): Version must be set to the exactly the same version as the one wasp is using: ^0.21.1
CallStack (from HasCallStack):
  error, called at src/Generator/WebAppGenerator.hs:73:31 in waspc-0.2.2.2-B3XOTY4be7eFFZyFqJsQfH:Generator.WebAppGenerator
```

The fix was to use the exact same version Wasp uses, as specified by the error message:
```
dependencies {=json
  "axios": "^0.21.1"
json=}

```

## 3) Dockerfile keeps getting overwritten. Any way to add to it? Do we want to use Docker Compose to allow for additional services like Redis?

## 4) Dockerfile python stale/heroku container does not support M1/aarch64 [`heroku container:push` did not work]

### First error: python missing (probably for all architectures, need to confirm)

```
 => [server-production 1/5] WORKDIR /app                                                                          0.0s
 => ERROR [server-builder 1/6] RUN apk add --no-cache build-base libtool autoconf automake python                 0.5s
------
 > [server-builder 1/6] RUN apk add --no-cache build-base libtool autoconf automake python:
#7 0.194 fetch https://dl-cdn.alpinelinux.org/alpine/v3.14/main/aarch64/APKINDEX.tar.gz
#7 0.366 fetch https://dl-cdn.alpinelinux.org/alpine/v3.14/community/aarch64/APKINDEX.tar.gz
#7 0.502 ERROR: unable to select packages:
#7 0.517   python (no such package):
#7 0.517     required by: world[python]
------
executor failed running [/bin/sh -c apk add --no-cache build-base libtool autoconf automake python]: exit code: 1
 ▸    Error: docker build exited with Error: 1
 ```

 Solution was to change base `python` to `python3`.

### Next errors: Prisma openssl related for M1

To bypass all of that, I had to manually build exclusively for x86 first like so: `docker buildx build --platform linux/amd64 . -t web`

I could then run:
```
docker tag web registry.heroku.com/waspleau/web
docker push registry.heroku.com/waspleau/web
```

Snip to save: ` docker buildx build --platform linux/amd64 . -t web && docker tag web registry.heroku.com/waspleau/web && docker push registry.heroku.com/waspleau/web && heroku container:release --app waspleau web`

## 5) Heroku postgres + prisma issue after changing sqlite to postgres

```
2021-12-16T02:18:51.366162+00:00 heroku[web.1]: Starting process with command `npm run start-production`
2021-12-16T02:18:52.504875+00:00 app[web.1]:
2021-12-16T02:18:52.504892+00:00 app[web.1]: > server@0.0.0 start-production /app/server
2021-12-16T02:18:52.504903+00:00 app[web.1]: > npm run db-migrate-prod && NODE_ENV=production node ./src/server.js "npm" "run" "start-production"
2021-12-16T02:18:52.504903+00:00 app[web.1]:
2021-12-16T02:18:52.793283+00:00 app[web.1]:
2021-12-16T02:18:52.793299+00:00 app[web.1]: > server@0.0.0 db-migrate-prod /app/server
2021-12-16T02:18:52.793300+00:00 app[web.1]: > prisma migrate deploy --schema=../db/schema.prisma
2021-12-16T02:18:52.793300+00:00 app[web.1]:
2021-12-16T02:18:53.589996+00:00 app[web.1]: Environment variables loaded from .env
2021-12-16T02:18:53.735979+00:00 app[web.1]: Prisma schema loaded from ../db/schema.prisma
2021-12-16T02:18:53.796395+00:00 app[web.1]: Datasource "db": PostgreSQL database "dbne5egge1dhr8", schema "public" at "ec2-184-73-25-2.compute-1.amazonaws.com:5432"
2021-12-16T02:18:53.963795+00:00 app[web.1]:
2021-12-16T02:18:53.963860+00:00 app[web.1]: 1 migration found in prisma/migrations
2021-12-16T02:18:54.208399+00:00 app[web.1]: Error: P3018
2021-12-16T02:18:54.208400+00:00 app[web.1]:
2021-12-16T02:18:54.208402+00:00 app[web.1]: A migration failed to apply. New migrations can not be applied before the error is recovered from. Read more about how to resolve migration issues in a production database: https://pris.ly/d/migrate-resolve
2021-12-16T02:18:54.208403+00:00 app[web.1]:
2021-12-16T02:18:54.208404+00:00 app[web.1]: Migration name: 20211208191826_dummy
2021-12-16T02:18:54.208404+00:00 app[web.1]:
2021-12-16T02:18:54.208404+00:00 app[web.1]: Database error code: 42601
2021-12-16T02:18:54.208405+00:00 app[web.1]:
2021-12-16T02:18:54.208405+00:00 app[web.1]: Database error:
2021-12-16T02:18:54.208406+00:00 app[web.1]: db error: ERROR: syntax error at or near "AUTOINCREMENT"
2021-12-16T02:18:54.208406+00:00 app[web.1]:
2021-12-16T02:18:54.208406+00:00 app[web.1]:
2021-12-16T02:18:54.220572+00:00 app[web.1]: npm ERR! code ELIFECYCLE
2021-12-16T02:18:54.220779+00:00 app[web.1]: npm ERR! errno 1
2021-12-16T02:18:54.224811+00:00 app[web.1]: npm ERR! server@0.0.0 db-migrate-prod: `prisma migrate deploy --schema=../db/schema.prisma`
2021-12-16T02:18:54.224850+00:00 app[web.1]: npm ERR! Exit status 1
2021-12-16T02:18:54.224925+00:00 app[web.1]: npm ERR!
2021-12-16T02:18:54.224925+00:00 app[web.1]: npm ERR! Failed at the server@0.0.0 db-migrate-prod script.
2021-12-16T02:18:54.224953+00:00 app[web.1]: npm ERR! This is probably not a problem with npm. There is likely additional logging output above.
2021-12-16T02:18:54.229064+00:00 app[web.1]:
2021-12-16T02:18:54.229202+00:00 app[web.1]: npm ERR! A complete log of this run can be found in:
2021-12-16T02:18:54.229274+00:00 app[web.1]: npm ERR!     /app/server/.npm/_logs/2021-12-16T02_18_54_225Z-debug.log
2021-12-16T02:18:54.250005+00:00 app[web.1]: npm ERR! code ELIFECYCLE
2021-12-16T02:18:54.250216+00:00 app[web.1]: npm ERR! errno 1
2021-12-16T02:18:54.254548+00:00 app[web.1]: npm ERR! server@0.0.0 start-production: `npm run db-migrate-prod && NODE_ENV=production node ./src/server.js "npm" "run" "start-production"`
2021-12-16T02:18:54.254618+00:00 app[web.1]: npm ERR! Exit status 1
2021-12-16T02:18:54.254695+00:00 app[web.1]: npm ERR!
2021-12-16T02:18:54.254741+00:00 app[web.1]: npm ERR! Failed at the server@0.0.0 start-production script.
2021-12-16T02:18:54.254783+00:00 app[web.1]: npm ERR! This is probably not a problem with npm. There is likely additional logging output above.
2021-12-16T02:18:54.258329+00:00 app[web.1]:
2021-12-16T02:18:54.258473+00:00 app[web.1]: npm ERR! A complete log of this run can be found in:
2021-12-16T02:18:54.258545+00:00 app[web.1]: npm ERR!     /app/server/.npm/_logs/2021-12-16T02_18_54_255Z-debug.log
2021-12-16T02:18:54.433588+00:00 heroku[web.1]: Process exited with status 1
2021-12-16T02:18:54.568000+00:00 heroku[web.1]: State changed from starting to crashed
2021-12-16T02:19:01.210559+00:00 app[api]: Deployed web (f5ce2d583e48) by user thelastemail@gmail.com
```

Had to delete and re-add database. Probably when I originally deployed with sqlite there it got into a funky state.

TODO(shayne): try to repro with simple, clean example and create a new Issue if it happens again.

## 6) .env file

Is seems the `.env` file is being copied into ./wasp/build/server? However, do we want to be copying over the `.env` at all? Prob ok given we set ENV on hosting provider.

| Bug captured here: https://github.com/wasp-lang/wasp/issues/413 |
| --------------------------------------------------------------- |

## 7) App loads, but only renders the string "Hello world"

Oh still need frontend deployed. Maybe instead of "Hello world" we note the frontend aspect somehow? Or as Martin noted we just say "I am api-server" or something like that

## Summary

I have created a new issue to track all PR comments and these notes to make sure we do not lose track of them.

| Captured here: https://github.com/wasp-lang/wasp/issues/514 |
| ----------------------------------------------------------- |