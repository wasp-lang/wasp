<!--
  Hi, thanks for contributing to Wasp!

  Comments like this one won't be shown in the final PR, but they contain
  instructions to guide you.

  Make sure to follow this PR template, so that we can speed up the review process.
  It will also help you not forget important steps when making a change.

  If you don't know how to fill any of the sections below, it's okay to leave them
  blank, we will help you out during the review.
-->

### Description

<!--
  Describe your PR!

  Common questions we'd like you to answer:
  - What's the motivation for this change?
  - Which changes are included in this PR?
    - If there are many different changes, consider splitting your PR into smaller
      ones. It will go through faster!

  If this PR closes an issue, write “Fixes #XXXX" so GitHub will link them together.

  You can also answer some of these questions if they are relevant:
  - Does this change affect users? How?
  - Have you considered any other approaches? Why is this one the best?
  - Are there any drawbacks or edge cases?
  - What are the possibilities for future work?
-->

### Select what type of change this PR introduces:

<!-- Put an x in between the brackets to select options, like so: [x] -->

- [ ] **Just code/docs improvement** (no functional change).
- [ ] **Bug fix** (non-breaking change which fixes an issue).
- [ ] **New feature** (non-breaking change which adds functionality).
- [ ] **Breaking change** (fix or feature that would cause existing functionality to not work as expected).

### Checklist

<!-- Put an x in between the brackets to select options, like so: [x] -->
<!-- You can add notes or explanations wherever needed -->

- 🧪 Testing:

  - [ ] I tested this change in a Wasp app **locally**.
  - [ ] <!-- If you modified Haskell code: --> I added **unit tests** for my change at `waspc/tests`.
  - [ ] <!-- If you added or updated a feature: --> I added an **integration test** for my change at `waspc/examples/todoApp/e2e-tests`.
  - [ ] <!-- If you added or updated a feature: --> I updated the **starters** at `waspc/data/Cli/templates`, if needed.
  - [ ] <!-- If you fixed a bug: --> I added a **regression test** for the bug I fixed.

- 📜 Documentation:

  - [ ] <!-- If you added a feature: --> I **added documentation** to the `web/docs/`, in a place that makes sense.
  - [ ] <!-- If you updated a feature: --> I **searched** for all relevant places in the `web/docs/` and **updated** them, if needed.

- 🆕 Changelog:

  <!-- If you did a bug fix, new feature, or breaking change: -->

  - [ ] I updated `waspc/ChangeLog.md` with a **user-friendly** description of the change.
  - [ ] I **bumped the `version`** in `waspc/waspc.cabal` to reflect changes I introduced.
  - [ ] <!-- If you did a breaking change: --> I added a step to the current **migration guide** at `web/docs/migration-guides/`.

  <!--
    While we're in beta, the version should be bumped according to the type of change:
      - Bug fix: patch version (0.0.X).
      - New feature: patch version (0.0.X).
      - Breaking change: minor version (0.X.0).
    If the version has already been bumped since the last release, you can skip this.
  -->

<!--
  Thanks for contributing! :)

  We'll check this PR as soon as we can. Meanwhile, keep an eye on this PR and
  fix any errors that might come up in the checks. It should be all green before
  we can merge it.
-->
