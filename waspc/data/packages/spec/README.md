The Wasp Spec, as an npm package.

**Note:** If you are a creating a Wasp app yourself, the Wasp CLI will install and manage this package for you. Use this package only if you are creating a Wasp Spec helper or library outside of a Wasp project.

## For Wasp users

If you are creating a Wasp app, you don't need to install this package yourself. The Wasp CLI will install and manage it for you.

- Learn more about Wasp: https://wasp.sh/
- Read about the Wasp spec: https://wasp.sh/docs/general/spec
- Follow our tutorial: https://wasp.sh/docs/tutorial/create
- Join our community: https://discord.gg/rzdnErX

## For Wasp Spec library authors

If you are creating a Wasp Spec helper or library outside of a Wasp project, you can install this package as a dependency in your project. For best results, add it to both your `devDependencies` and `peerDependencies` in your `package.json`, so you have a local copy during development, but your library users will use the version of the package that is installed in their Wasp project.

In `package.json`:

```json
{
  "peerDependencies": {
    "@wasp/spec": "^0.24.0"
  },
  "devDependencies": {
    "@wasp/spec": "^0.24.0"
  }
}
```
