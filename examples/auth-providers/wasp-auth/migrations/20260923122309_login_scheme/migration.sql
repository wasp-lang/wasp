/*
  Warnings:

  - You are about to drop the column `signedInBy` on the `OneTimeCode` table. All the data in the column will be lost.
  - You are about to drop the column `signedInBy` on the `Session` table. All the data in the column will be lost.
  - Added the required column `loginScheme` to the `OneTimeCode` table without a default value. This is not possible if the table is not empty.
  - Added the required column `loginScheme` to the `Session` table without a default value. This is not possible if the table is not empty.

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
CREATE TABLE "new_Session" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "expiresAt" DATETIME NOT NULL,
    "issuedAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "loginScheme" TEXT NOT NULL,
    "userId" TEXT NOT NULL,
    CONSTRAINT "Session_userId_fkey" FOREIGN KEY ("userId") REFERENCES "Auth" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);
INSERT INTO "new_Session" ("expiresAt", "id", "issuedAt", "userId") SELECT "expiresAt", "id", "issuedAt", "userId" FROM "Session";
DROP TABLE "Session";
ALTER TABLE "new_Session" RENAME TO "Session";
CREATE UNIQUE INDEX "Session_id_key" ON "Session"("id");
CREATE INDEX "Session_userId_idx" ON "Session"("userId");
PRAGMA foreign_keys=ON;
PRAGMA defer_foreign_keys=OFF;
