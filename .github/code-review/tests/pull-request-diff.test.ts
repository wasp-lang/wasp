import assert from "node:assert/strict";
import test from "node:test";
import {
  isFindingOnChangedLines,
  parsePullRequestDiff,
} from "../pull-request-diff.ts";

const diff = parsePullRequestDiff(`diff --git a/src/example.ts b/src/example.ts
index 1111111..2222222 100644
--- a/src/example.ts
+++ b/src/example.ts
@@ -1,2 +1,3 @@
 const before = true;
+const added = true;
 const after = true;
`);

test("accepts a finding ending on an added line", () => {
  assert.equal(
    isFindingOnChangedLines(
      {
        body: "Description",
        suggestion: null,
        path: "src/example.ts",
        startLine: 1,
        endLine: 2,
      },
      diff,
    ),
    true,
  );
});

test("rejects a finding outside the changed hunk", () => {
  assert.equal(
    isFindingOnChangedLines(
      {
        body: "Description",
        suggestion: null,
        path: "src/example.ts",
        startLine: 20,
        endLine: 20,
      },
      diff,
    ),
    false,
  );
});

test("treats added content beginning with pluses as content", () => {
  const plusContentDiff = parsePullRequestDiff(`diff --git a/notes.md b/notes.md
--- a/notes.md
+++ b/notes.md
@@ -0,0 +1 @@
+++ heading
`);

  assert.equal(
    isFindingOnChangedLines(
      {
        body: "Description",
        suggestion: null,
        path: "notes.md",
        startLine: 1,
        endLine: 1,
      },
      plusContentDiff,
    ),
    true,
  );
});

test("decodes non-ASCII Git paths", () => {
  const quotedPathDiff =
    parsePullRequestDiff(`diff --git "a/src/\\303\\244.ts" "b/src/\\303\\244.ts"
--- "a/src/\\303\\244.ts"
+++ "b/src/\\303\\244.ts"
@@ -0,0 +1 @@
+export {};
`);

  assert.equal(
    isFindingOnChangedLines(
      {
        body: "Description",
        suggestion: null,
        path: "src/ä.ts",
        startLine: 1,
        endLine: 1,
      },
      quotedPathDiff,
    ),
    true,
  );
});
