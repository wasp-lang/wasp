---
title: Telemetry
---

## Overview

The term **telemetry** refers to the collection of certain usage data to help improve the quality of a piece of software (in this case, Wasp).

Our telemetry implementation is anonymized and very limited in its scope, focused on answering following questions:

- How many people and how often: tried to install Wasp, use Wasp, have built a Wasp app, or have deployed one?
- How many projects are created with Wasp?

## When and what is sent?

- Information is sent via HTTPS request when `wasp` CLI command is invoked.
  Information is sent no more than twice in a period of 12 hours (sending is paused for 12 hours after last invocation, separately for `wasp build` command and for all other commands). Exact information as it is sent:

  ```json
  {
    // Randomly generated, non-identifiable UUID representing a user.
    "distinct_id": "bf3fa7a8-1c11-4f82-9542-ec1a2d28786b",
    // Non-identifiable hash representing a project.
    "project_hash": "6d7e561d62b955d1",
    // True if command was `wasp build`, false otherwise.
    "is_build": true,
    // Captures `wasp deploy ...` args, but only those from the limited, pre-defined list of keywords.
    // Those are "fly", "railway", "setup", "create-db", "deploy", "cmd", and "launch". Everything else is ommited.
    "deploy_cmd_args": "fly;deploy",
    "wasp_version": "0.1.9.1",
    "os": "linux",
    // "CI" if running on CI, and whatever is the content of "WASP_TELEMETRY_CONTEXT" env var.
    // We use this to track when execution is happening in some special context, like on Gitpod, Replit or similar.
    "context": "CI"
  }
  ```

- Information is also sent once via HTTPS request when wasp is installed via `install.sh` script. Exact information as it is sent:
  ```json
  {
    // Randomly generated id.
    "distinct_id": "274701613078193779564259",
    "os": "linux"
  }
  ```

## Opting out

You sharing the telemetry data with us means a lot to us, since it helps us understand how popular Wasp is, how it is being used, how the changes we are doing affect usage, how many new vs old users there are, and just in general how Wasp is doing. We look at these numbers every morning and they drive us to make Wasp better.

However, if you wish to opt-out of telemetry, we understand!
You can do so by setting the `WASP_TELEMETRY_DISABLE` environment variable to any value, e.g.:

```
export WASP_TELEMETRY_DISABLE=1
```

## Future plans

We don't have this implemented yet, but the next step will be to make telemetry go in two directions -> instead of just sending usage data to us, it will also at the same time check for any messages from our side (e.g. notification about new version of Wasp, or a security notice). [Link to corresponding github issue](https://github.com/wasp-lang/wasp/issues/163).
