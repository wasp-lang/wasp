# `fetch-nightly-cli` action

A GitHub Action that fetches the latest nightly CLI build from Wasp's CI runs. This action enables testing against the latest version of Wasp from `main` or from a specific branch/PR.

> [!TIP]
> This action is intended for internal usage only. If you have a Wasp app and you're just looking to install Wasp in your CI, you can use this step instead:
>
> ```yaml
> - run: curl -sSL https://get.wasp.sh/installer.sh | sh -- -v [version]
> ```

## Background

This action provides an internal "nightly" system for Wasp, allowing other repositories and workflows to download and install the binary from the latest successful CI run. The action:

- Fetches the most recent successful build from the specified branch (defaults to `main`).
- Downloads the appropriate binary for the current OS and architecture.
- Can be used to test against PRs by specifying a different branch.

## Usage

### Basic example

Fetch the latest nightly build from `main` and install it:

```yaml
- uses: wasp-lang/wasp/.github/actions/fetch-nightly-cli@main
- run: curl -sSL https://get.wasp.sh/installer.sh | sh -- -f wasp-*.tar.gz
```

The wildcard `wasp-*.tar.gz` is used because the artifact has a different name depending on the OS and architecture. The `-f` flag tells the installer to use the locally downloaded file.

### Fetch from a specific branch

To test against a specific branch or PR:

```yaml
- uses: wasp-lang/wasp/.github/actions/fetch-nightly-cli@main
  with:
    branch: my-feature-branch
- run: curl -sSL https://get.wasp.sh/installer.sh | sh -- -f wasp-*.tar.gz
```

### Custom output directory

Download the CLI package to a specific directory:

```yaml
- uses: wasp-lang/wasp/.github/actions/fetch-nightly-cli@main
  with:
    output-dir: ./bin
- run: curl -sSL https://get.wasp.sh/installer.sh | sh -- -f bin/wasp-*.tar.gz
```
