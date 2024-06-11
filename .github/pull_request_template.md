### Description

> Describe your PR! If it fixes specific issue, mention it with "Fixes # (issue)".

### Select what type of change this PR introduces:
1. [ ] **Just code/docs improvement** (no functional change).
2. [ ] **Bug fix** (non-breaking change which fixes an issue).
3. [ ] **New feature** (non-breaking change which adds functionality).
4. [ ] **Breaking change** (fix or feature that would cause existing functionality to not work as expected).

### Update Waspc ChangeLog and version if needed
If you did a bug fix, new feature, or breaking change, that affects waspc, make sure you satisfy the following:
1. [ ] I updated [ChangeLog.md](https://github.com/wasp-lang/wasp/blob/main/waspc/ChangeLog.md) with description of the change this PR introduces.
2. [ ] I bumped waspc version in [waspc.cabal](https://github.com/wasp-lang/wasp/blob/main/waspc/waspc.cabal) to reflect changes I introduced, with regards to the version of the latest wasp release, if the bump was needed.

### Update example apps if needed
If you did code changes and added a new feature or modified an existing feature, make sure you satisfy the following:
1. [ ] I updated `waspc/examples/todoApp` as needed (updated modified feature or added new feature) and manually checked it works correctly.
2. [ ] I updated `waspc/headless-test/examples/todoApp` and its e2e tests as needed (updated modified feature and its tests or added new feature and new tests for it).
