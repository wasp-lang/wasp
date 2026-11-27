> Fetch the complete documentation index at: https://wasp.sh/llms.txt
---

# Legacy Wasp installer

:::note
Last checked with Wasp 0.27.

This guide depends on external libraries or services, so it may become outdated over time. We do our best to keep it up to date, but make sure to check their documentation for any changes.
:::

Since Wasp 0.21, you install Wasp through npm, and since Wasp 0.27, each project installs its own Wasp as an npm dependency. The installer script is legacy, and we don't support it anymore. We'll keep it around for the foreseeable future to give you time to switch, but it can't install Wasp 0.21 or newer.

## How to migrate off the legacy installer {#migrate}

1. Remove the installer's Wasp with our migration tool:

   ```shell
   curl -sSL https://get.wasp.sh/installer.sh | sh -s -- migrate-to-npm
   ```

2. Install the Wasp CLI globally from npm, on your project's Wasp version. npm has Wasp 0.20.2 and newer:

   ```shell
   # Set x.y.z to your project's Wasp version.
   npm i -g @wasp.sh/wasp-cli@x.y.z
   ```

   For a project older than Wasp 0.20.2, see [I need to use versions older than Wasp 0.20.2](#older-versions).

3. Upgrade the project to the latest Wasp. Follow the [migration guides](https://wasp.sh/docs/migration-guide) one version at a time, starting with the one from your project's version.

   Notable releases on the way:

   - [Wasp 0.21](https://wasp.sh/docs/0.21/migration-guide): Wasp moved from the installer script to npm.
   - [Wasp 0.27](https://wasp.sh/docs/migration-guide): each project installs its own Wasp as an npm dependency, and the global Wasp CLI is `@wasp/cli`. Read more in [Running Wasp commands](https://wasp.sh/docs/advanced/cli#running-wasp-commands).

## Keep using the legacy installer {#keep-using}

If you haven't yet migrated to the npm installer method, you can keep using the legacy installer, by running:

```shell
# Set x.y.z to the version you want to install, e.g. 0.20.1
# The installer will refuse to run without a specific version argument
curl -sSL https://get.wasp.sh/installer.sh | sh -s -- -v x.y.z
```

You should only use the legacy installer as a stopgap while migrating workstations and CI to npm installations, as it can cause conflicts with the new method. For that reason, the installer will not work in the following cases:

- You have already installed Wasp 0.21 to 0.26 through npm (`@wasp.sh/wasp-cli`).
- You have already run the migration tool.
- You are trying to install Wasp >= 0.21.
- You are calling the installer without a version argument.

In any of these cases, the installer will print an error message and exit without installing Wasp. You can still [switch back manually](#switch-back) if needed.

### With Wasp 0.27 or newer installed through npm {#both-installed}

Since Wasp 0.27, the npm Wasp CLI (`@wasp/cli`) and the installer don't check for each other, so you can end up with both. Your shell then runs whichever `wasp` comes first in your `PATH`, which might not be the one you expect. List them with:

```shell
type -a wasp
```

A `wasp` in `~/.local/bin` is the installer's. [Migrate off the legacy installer](#migrate) to remove it. Inside a project, `npm run wasp` always runs the project's own Wasp.

## Troubleshooting

### I need to use versions older than Wasp 0.20.2 {#older-versions}

These versions are very out of date and we don't recommend using them. We urge you to upgrade your Wasp project to a newer version as soon as possible to keep your app secure and stable.

In the transition period until these apps are migrated, you will need to [keep using the legacy installer](#keep-using) to install these older versions of Wasp.

### I need to switch back to the legacy installer {#switch-back}

:::note
If you found a bug in the npm-based Wasp, or a workflow that is no longer possible, please report it to us so we can fix it as soon as possible. You can do that [through a GitHub issue](https://github.com/wasp-lang/wasp/issues/new/choose), or [on our Discord server](https://discord.gg/rzdnErX).
:::

If you have already switched to npm installation but need to switch back to the legacy installer:

1. Uninstall the npm version of Wasp:

   ```shell
   # Wasp 0.27 and newer:
   npm uninstall -g @wasp/cli

   # Wasp 0.20.2 to 0.26.x:
   npm uninstall -g @wasp.sh/wasp-cli
   ```

2. Make sure that Wasp is uninstalled from your system:

   ```shell
   type wasp
   ```

   If Wasp was correctly uninstalled, the `type` command will output "not found". If Wasp has not been completely uninstalled, it will print the path of the `wasp` binary, and you can manually remove it.

3. Remove the npm marker file from your system. Only Wasp 0.21 to 0.26 create it:

   ```shell
   rm -f $HOME/.local/share/wasp-lang/.uses-npm
   ```

4. Run the installer again with the version you need:

   ```shell
   # Set x.y.z to the version you want to install, e.g. 0.20.1
   # The installer will refuse to run without a specific version argument
   curl -sSL https://get.wasp.sh/installer.sh | sh -s -- -v x.y.z
   ```

### "Bad CPU type in executable" on Mac with Mx chip (Apple Silicon)

You have two options to run Wasp on your Mac with Mx chip:

1. **Recommended:** [Migrate to the npm-based installation method](#migrate), which works natively on Apple Silicon.

2. Keep using the legacy installer, but install [Rosetta on your Mac](https://support.apple.com/en-us/HT211861) to enable running x86 binaries.

   To install Rosetta, run the following command in your terminal

   ```bash
   softwareupdate --install-rosetta
   ```

   Once installed, Wasp will run on your system as normal.