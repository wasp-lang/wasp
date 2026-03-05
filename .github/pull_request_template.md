<!--
  Thanks for contributing to Wasp!
  Make sure to follow this PR template, so that we can speed up the review process.
  It will also help you not forget important steps when making a change.
  If you don't know how to fill any of the sections below, it's okay to leave
  them blank and ask for help.
-->

## Description

Replace this message here, and write a high-level overview with any additional
context (motivation, trade-offs, approaches considered, concerns, ...).

## Type of change

<!-- Select just one with [x] -->

- [ ] **🔧 Just code/docs improvement** <!-- no functional change -->
- [ ] **🐞 Bug fix** <!-- non-breaking change which fixes an issue -->
- [ ] **🚀 New/improved feature** <!-- non-breaking change which adds functionality -->
- [ ] **💥 Breaking change** <!-- fix or feature that would cause existing functionality to not work as expected -->

## Checklist

<!--
  Check all the applicable boxes with [x], and leave the rest empty.
  If you're unsure about any of them, don't hesitate to ask for help.
-->

- [ ] I tested my change in a Wasp app to verify that it works as intended.

- 🧪 Tests and apps:

  - [ ] I added **unit tests** for my change. <!-- If not, explain why. -->
  - [ ] _(if you fixed a bug)_ I added a **regression test** for the bug I fixed. <!-- If not, explain why. -->
  - [ ] _(if you added/updated a feature)_ I added/updated **e2e tests** in `examples/kitchen-sink/e2e-tests`.
  - [ ] _(if you added/updated a feature)_ I updated the **starter templates** in `waspc/data/Cli/templates`, as needed.
  - [ ] _(if you added/updated a feature)_ I updated the **example apps** in `examples/`, as needed.
    - [ ] _(if you updated `examples/tutorials`)_ I updated the tutorial in the docs (and vice versa).

- 📜 Documentation:

  - [ ] _(if you added/updated a feature)_ I **added/updated the documentation** in `web/docs/`.

- 🆕 Changelog: _(if change is more than just code/docs improvement)_
  - [ ] I updated `waspc/ChangeLog.md` with a **user-friendly** description of the change.
  - [ ] _(if you did a breaking change)_ I added a step to the current **migration guide** in `web/docs/migration-guides/`.
  - [ ] I **bumped the `version`** in `waspc/waspc.cabal` to reflect the changes I introduced.

<!--
  Bumping the version on `waspc/waspc.cabal`:

  We still haven't reached 1.0, so the version bumping follows these rules:
    - Bug fix:            0.X.+1    (e.g. 0.16.3 bumps to 0.16.4)
    - New feature:        0.X.+1    (e.g. 0.16.3 bumps to 0.16.4)
    - Breaking change:    0.+1.0    (e.g. 0.16.3 bumps to 0.17.0)

  If the version has already been bumped on `main` since the last release, skip this.
-->
