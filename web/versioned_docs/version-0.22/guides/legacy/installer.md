---
sidebar_label: "Wasp Installer"
comments: true
last_checked_with_versions:
  Wasp: 0.21
---

# Legacy Wasp installer

Starting from Wasp 0.21, installation is done [through npm](../../introduction/quick-start.md#detailed-installation). The installation method using the script installer is now considered legacy and is not supported anymore. We'll keep it around for the foreseeable future to give users time to switch, but you will not be able to get newer versions until you migrate to npm-based installation.

## How to migrate off the legacy installer {#migrate}

To switch to the new installation method, you can run our migration tool:

```shell
curl -sSL https://get.wasp.sh/installer.sh | sh -s -- migrate-to-npm
```

Afterwards, you can use [the regular installation instructions](../../introduction/quick-start.md#detailed-installation) to install Wasp through npm:

```shell
npm i -g @wasp.sh/wasp-cli@latest
```

You can also ask for a specific version of Wasp. Wasp versions 0.20.2 and greater are available through npm:

```shell
# Set x.y.z to the version you want to install, e.g. 0.20.2
npm i -g @wasp.sh/wasp-cli@x.y.z
```

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
