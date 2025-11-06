<!--
  Thanks for contributing to Wasp!
  Make sure to follow this PR template, so that we can speed up the review process.
  It will also help you not forget important steps when making a change.
  If you don't know how to fill any of the sections below, it's okay to leave
  them blank and ask for help.
-->

## Description

<!--
  Write a high-level overview and any additional context (motivation, trade-offs,
  approaches considered, concerns, ...)
-->

TODO

## Type of the change

<!-- Select just one, the largest change: -->

- [ ] `v _._._` **Just code/docs improvement** <!-- no functional change -->
- [ ] `v _._.+1` **Bug fix** <!-- non-breaking change which fixes an issue -->
- [ ] `v _._.+1` **New/improved feature** <!-- non-breaking change which adds functionality -->
- [ ] `v _.+1.0` **Breaking change** <!-- fix or feature that would cause existing functionality to not work as expected -->

## Checklist

<!--
  Check the relevant boxes, and strikethrough those that do not apply.
  We prefer that you explain if something is not applicable, rather than leaving
  it unchecked.
-->

- [ ] I tested my change in a Wasp app to verify that it works as intended.

- 🧪 Tests:
  - [ ] I added **unit tests** for my change. <!-- If not, explain why. -->
  - [ ] _(if you fixed a bug)_ I added a **regression test** for the bug I fixed. <!-- If not, explain why. -->
  - [ ] _(if you added/updated a feature)_ I added/updated **e2e tests** at `waspc/examples/todoApp/e2e-tests`.
  - [ ] _(if you added/updated a feature)_ I updated the **starter templates** at `waspc/data/Cli/templates`, as needed.

- 📜 Documentation:
  - [ ] _(if you added/updated a feature)_ I **added/updated the documentation** in `web/docs/`.

- 🆕 Changelog: _(if change is more than just code/docs improvement)_
  - [ ] I updated `waspc/ChangeLog.md` with a **user-friendly** description of the change.
  - [ ] I **bumped the `version`** in `waspc/waspc.cabal` to reflect the changes I introduced.
  - [ ] _(if you did a breaking change)_ I added a step to the current **migration guide** at `web/docs/migration-guides/`.

<!--
  On updating the waspc version in waspc/waspc.cabal:
  We still haven't reached 1.0, so the version bumping follows these rules:
    - Bug fix or new feature: 0.X.(Y+1)
    - Breaking change: 0.(X+1).0
  where 0.X.Y is the version of the last release.

  If the version has already been bumped as needed on `main` branch since the
  last release, skip this.
-->
