export const singleFilePatch = `diff --git a/test.js b/test.js
index 1234567..abcdefg 100644
--- a/test.js
+++ b/test.js
@@ -1,3 +1,4 @@
 console.log('hello');
+console.log('world');
 export const x = 1;
 export const y = 2;`;

export const multiFilePatch = `diff --git a/file1.js b/file1.js
index 1234567..abcdefg 100644
--- a/file1.js
+++ b/file1.js
@@ -1,2 +1,3 @@
 export const a = 1;
+export const b = 2;
 export { a };
diff --git a/file2.js b/file2.js
index 2345678..bcdefgh 100644
--- a/file2.js
+++ b/file2.js
@@ -1,2 +1,3 @@
 export const c = 3;
+export const d = 4;
 export { c };`;

export const deletionPatch = `diff --git a/deleted-file.js b/deleted-file.js
deleted file mode 100644
index 1234567..0000000
--- a/deleted-file.js
+++ /dev/null
@@ -1,3 +0,0 @@
-console.log('deleted');
-export const x = 1;
-export { x };`;

export const additionPatch = `diff --git a/new-file.js b/new-file.js
new file mode 100644
index 0000000..1234567
--- /dev/null
+++ b/new-file.js
@@ -0,0 +1,3 @@
+console.log('new file');
+export const y = 2;
+export { y };`;

export const binaryPatch = `diff --git a/image.png b/image.png
index 1234567..abcdefg 100644
Binary files a/image.png and b/image.png differ`;

export const noFilesPatch = `commit abc123
Author: Test User
Date: Mon Jan 1 00:00:00 2023 +0000

    Some commit message`;
