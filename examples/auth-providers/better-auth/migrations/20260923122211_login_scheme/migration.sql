/*
  Warnings:

  - You are about to drop the column `signedInBy` on the `OneTimeCode` table. All the data in the column will be lost.
  - Added the required column `loginScheme` to the `OneTimeCode` table without a default value. This is not possible if the table is not empty.

*/
-- RedefineTables
PRAGMA defer_foreign_keys=ON;
PRAGMA foreign_keys=OFF;
CREATE TABLE "new_OneTimeCode" (
    "code" TEXT NOT NULL PRIMARY KEY,
    "authId" TEXT NOT NULL,
    "loginScheme" TEXT NOT NULL,
    "expiresAt" DATETIME NOT NULL,
    "usedAt" DATETIME
);
INSERT INTO "new_OneTimeCode" ("authId", "code", "expiresAt", "usedAt") SELECT "authId", "code", "expiresAt", "usedAt" FROM "OneTimeCode";
DROP TABLE "OneTimeCode";
ALTER TABLE "new_OneTimeCode" RENAME TO "OneTimeCode";
PRAGMA foreign_keys=ON;
PRAGMA defer_foreign_keys=OFF;
