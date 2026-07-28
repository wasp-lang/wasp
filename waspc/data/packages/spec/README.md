The Wasp Spec, as an npm package.

## For Wasp users

If you are creating a Wasp app, you don't need to install this package yourself. The Wasp CLI will install and manage it for you.

- Learn more about Wasp: https://wasp.sh/
- Read the docs: https://wasp.sh/docs
- Read about the Wasp spec: https://wasp.sh/docs/general/spec
- Browse the API reference: https://wasp.sh/docs/api/@wasp.sh/spec
- Join our community: https://discord.gg/rzdnErX

## For Wasp Spec library authors

If you are creating a Wasp Spec helper or library outside of a Wasp project, you can install this package as a dependency in your project. For best results, add it to both your `devDependencies` and `peerDependencies` in your `package.json`, so you have a local copy during development, but your library users will use the version of the package that is installed in their Wasp project.

In `package.json`:

```json
{
  "peerDependencies": {
    "@wasp.sh/spec": "^0.25.0"
  },
  "devDependencies": {
    "@wasp.sh/spec": "^0.25.0"
  }
}
```
