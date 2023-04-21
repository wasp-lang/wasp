### Take into account things

1. `railway login` does not work in non-interactive mode. Current implementation checks if user logged in, otherwise throw out of the process with a friendly hint what to do next
2. Global options `--wasp-exe <path>` and `--wasp-project-dir <dir>` are being added to each subcommand in the global scope
3. This implementation expects a specific file for railway deployments to be present in the main root directory of Wasp - `wasp.deployrc.railway.json`
   It's expected signature is:

```
	"environment": "production",
	"projectId": "bef41c2c-b1a9-42ef-adb9-e3afa84fbbea",
	"clientService": {
		"name": "client",
		"url": "client-production-6ea1.up.railway.app"
	},
	"serverService": {
		"name": "server",
		"url": "server-production-dfaf.up.railway.app"
	}
```

Environment - required by Railway as there can exist multiple envs
ProjectID - build folder has to be linked with a respective project
Client / Server Service - info about the client service (ID and its URL). Railways projects consist of services, that you can spin up pretty easily. Each service has its own env variables etc.

Whenever "setup" feature is implemented, we can ensure that this file is create automatically, but I am not entirely sure if that's a good idea to do. Need peoplez thoughts on this. Alternative, we force users to provide those values as flags / arguments.. ?

### Refactor suggestions things

4. Suggesting to keep all types in `.d.ts` files to clearly indicate what they are
5. Even though haven't done it properly myself, but I'd probably also remove Capital casing on type files, as I see no benefit in this, if we mark the file with `.d.ts` extension, it should be clear enough what file is for.
6. Stick to naming convention of types to indicate where they belong to - FlyDeployOptions, Railway(Rw)DeployOptions. Eases search of specific options across the codebase instead of doing a global search and having 10+ same names
7. Installed `fs-extra` package, hoping it's allowed :) otherwise, can be reimplemented (mainly used for `ensureDir`, but it provides other convenient stuffs)

8. `await $railway up --service ${clientService.name}` - is a stream of logs for deploying. Atm this implementation just fires & forgets, giving a URL of logs as an output, so maybe for v1 it's ok, but later do a re-stream? No idea how to re-stream output logs tho

9. Fn `HACK_PACKAGES_JSON` has to die, but it allows to continue with implementation, as types are not used in Docker and `tsc` is run, that leads to bunch of types missing. This is because Server is not built inside the Docker container, so I'd like opinions on what could be / should be done. It might come across as pretty obvious one - just build the server - but do we then cherry pick specific folders from server, aiming just specific types folders etc? I am not fan of this option because if we add a library tomorrow to the server side, and forget to cherry pick its types inside this deployment Docker, we will have a failing build.
10. Added prettier config for a consist styling, feedback / suggestions on its content is appreciated! For now, I just tried to mirror what has already been put together in terms of style by Shayne :)

A lot has been moved around from `/fly` provider, as a lot of nice conveniences were living in it, so had to move it up a scope to make them shareable.
