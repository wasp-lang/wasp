/*
  Warnings:

  - You are about to drop the `Session` table. If the table is not empty, all the data it contains will be lost.

*/
-- AlterTable
ALTER TABLE "Auth" ADD COLUMN "credentialsInvalidatedAt" DATETIME;

-- DropTable
PRAGMA foreign_keys=off;
DROP TABLE "Session";
PRAGMA foreign_keys=on;

-- CreateTable
CREATE TABLE "UsedOneTimeCode" (
    "code" TEXT NOT NULL PRIMARY KEY,
    "usedAt" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP
);
