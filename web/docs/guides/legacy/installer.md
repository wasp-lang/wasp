---
title: "Legacy Wasp installer: migrate to npm installation"
description: Since Wasp 0.21, you install Wasp through npm, and since Wasp 0.27, each project installs it as a dependency. Migrate off the legacy installer script, or keep a global Wasp CLI.
sidebar_label: "Wasp Installer"
comments: true
---

import LastCheckedWithVersionsNotice from "@site/src/components/LastCheckedWithVersionsNotice";
import Trackable from "@site/src/components/Trackable";

# Legacy Wasp installer

<LastCheckedWithVersionsNotice versions={{ Wasp: "0.27" }} />

Since Wasp 0.21, you install Wasp [through npm](../../getting-started/quick-start.md#detailed-installation). The installer script is legacy, and we don't support it anymore. We'll keep it around for the foreseeable future to give you time to switch, but it can't install Wasp 0.21 or newer.

## How to migrate off the legacy installer {#migrate}

To switch away from the installer script, run our migration tool:

```shell
curl -sSL https://get.wasp.sh/installer.sh | sh -s -- migrate-to-npm
```

Since Wasp 0.27, each project installs Wasp as an npm dependency, and that's the default way to use Wasp. The `@wasp/wasp` version in the project's `package.json` sets its Wasp version, `npm install` installs it, and you run it through the project's npm scripts, like `npm run dev`. Read more in [Running Wasp commands](../../advanced/cli.md#running-wasp-commands).

To create a new project without installing anything globally, run:

```shell
npx @wasp/cli@latest new
```

If your project runs Wasp 0.26 or older, follow the [migration guide](../../migration-guide.md) to make it depend on `@wasp/wasp`.

### Keep using a global Wasp CLI

If you'd like to keep typing `wasp` commands like you did with the installer script, install the Wasp CLI globally through npm:

<Trackable event="Install Command: Copy">

```shell
npm i -g @wasp/cli@latest
```

</Trackable>

Inside a project, the global CLI runs the project's own Wasp, so you don't need to match its version to your projects. [Installing Wasp globally](../../getting-started/quick-start.md#global-cli) explains how it picks the version.

Projects on Wasp 0.26 or older don't depend on `@wasp/wasp`, so you need a global CLI of the same version to work on them. npm has Wasp 0.20.2 and newer:

<Trackable event="Install Command: Copy">

```shell
# Set x.y.z to the version you want to install.
# Wasp 0.27 and newer:
npm i -g @wasp/cli@x.y.z

# Wasp 0.20.2 to 0.26.x:
npm i -g @wasp.sh/wasp-cli@x.y.z
```

</Trackable>

## Keep using the legacy installer {#keep-using}

If you haven't yet migrated to the npm installer method, you can keep using the legacy installer, by running:

```shell
# Set x.y.z to the version you want to install, e.g. 0.20.1
# The installer will refuse to run without a specific version argument
curl -sSL https://get.wasp.sh/installer.sh | sh -s -- -v x.y.z
```

You should only use the legacy installer as a stopgap while migrating workstations and CI to npm installations, as it can cause conflicts with the new method. For that reason, the installer will not work in the following cases:

- You have already installed Wasp through npm.
- You have already run the migration tool.
- You are trying to install Wasp >= 0.21.
- You are calling the installer without a version argument.

In any of these cases, the installer will print an error message and exit without installing Wasp. You can still [switch back manually](#switch-back) if needed.

## Troubleshooting

### I need to use versions older than Wasp 0.20.2

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

3. Remove the npm marker file from your system:

    ```shell
    rm $HOME/.local/share/wasp-lang/.uses-npm
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
