### Description

> Describe your PR! If this PR closes an issue, use “Fixes #(issue_number)" syntax so GitHub will auto-close it when merged.

### Select what type of change this PR introduces:

1. [ ] **Just code/docs improvement** (no functional change).
2. [ ] **Bug fix** (non-breaking change which fixes an issue).
3. [ ] **New feature** (non-breaking change which adds functionality).
4. [ ] **Breaking change** (fix or feature that would cause existing functionality to not work as expected).

### Update Waspc ChangeLog and version if needed

If you did a **bug fix, new feature, or breaking change**, that affects `waspc`, make sure you satisfy the following:

1. [ ] I updated [`ChangeLog.md`](https://github.com/wasp-lang/wasp/blob/main/waspc/ChangeLog.md) with description of the change this PR introduces.
2. [ ] I bumped `waspc` version in [`waspc.cabal`](https://github.com/wasp-lang/wasp/blob/main/waspc/waspc.cabal) to reflect changes I introduced, with regards to the version of the latest wasp release, if the bump was needed.

### Add a regression test if needed

If you did a **bug fix**, make sure you satisfy the following:

1. [ ] I added a regression test that reproduces the bug and verifies the fix.

If you're unable to add a regression test, please explain why.
This likely indicates that our current testing setup needs improvement.

### Test Coverage

Please ensure your changes are adequately tested:

1. [ ] **My changes are covered by tests** (unit, integration, or e2e tests as appropriate).

If you're unable to add tests or if coverage is partial, please explain why below:

<!-- Provide explanation here if tests are missing or incomplete -->

### Update example apps if needed

If you did code changes and **added a new feature**, make sure you satisfy the following:

1. [ ] I updated [`waspc/examples/todoApp`](https://github.com/wasp-lang/wasp/tree/main/waspc/examples/todoApp) and its e2e tests as needed and manually checked it works correctly.

If you did code changes and **updated an existing feature**, make sure you satisfy the following:

1. [ ] I updated [`waspc/examples/todoApp`](https://github.com/wasp-lang/wasp/tree/main/waspc/examples/todoApp) and its e2e tests as needed and manually checked it works correctly.

### Update starter apps if needed

If you did code changes and **updated an existing feature**, make sure you satisfy the following:

1. [ ] I updated [starter skeleton](https://github.com/wasp-lang/wasp/tree/main/waspc/data/Cli/templates/skeleton) as needed and manually checked it works correctly.
2. [ ] I updated [`basic` starter](https://github.com/wasp-lang/wasp/tree/main/waspc/data/Cli/templates/basic) as needed and manually checked it works correctly.
3. [ ] I updated [`todo-ts` starter](https://github.com/wasp-lang/starters/tree/dev/todo-ts) as needed and manually checked it works correctly.
4. [ ] I updated [`embeddings` starter](https://github.com/wasp-lang/starters/tree/dev/embeddings) as needed and manually checked it works correctly.
5. [ ] I updated [`saas` starter](https://github.com/wasp-lang/open-saas/tree/main/template) as needed and manually checked it works correctly.

### Update e2e tests if needed

If you did code changes and changed Wasp's code generation logic, make sure you satisfy the following:

1. [] I updated [e2e tests](https://github.com/wasp-lang/wasp/tree/main/waspc#end-to-end-e2e-tests) as needed and manually checked they are correct.
