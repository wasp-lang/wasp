More of a TODO list probably, to note down few things that are known "gotchas" during implementation.
To revisit when PR is created.

### Todo things

1. `railway login` does not work in non-interactive mode. Current impl asks for options to login or not atm, so just remove it if there is no way around it and force user to login outside of the one-liner.
2. Revisit "global options", like --wasp-exe <path> stuff || Attached to each command in global mode
3. Do not forget to clean up `package.json` with personal args..

### Suggestions

4. Suggesting to keep all types in `.d.ts` files to clearly indicate what they are
5. Stick to naming convention of types to indicate what they belong to - FlyDeployOptions, Railway(Rw)DeployOptions. Eases search of specific options across the codebase instead of doing a global search and having 10+ same names
6. Installed `fs-extra` package, hoping it's allowed :) otherwise, can be reimplemented (mainly used for `ensureDir`, but it provides other convenient stuffs)
7. `await $railway up --service ${clientService.name}` - is a stream of logs for deploying. Atm this implementation just fires & forgets, giving a URL of logs as an output, so maybe for v1 it's ok, but later do a re-stream? No idea how to re-stream output logs tho
8. Fn `HACK_PACKAGES_JSON` has to die, but it allows to continue with implementation, as types are not used in Docker and `tsc` is run, that leads to bunch of types missing
