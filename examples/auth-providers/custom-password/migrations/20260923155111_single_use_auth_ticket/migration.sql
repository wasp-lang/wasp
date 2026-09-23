/*
  Warnings:

  - You are about to drop the `OneTimeCode` table. If the table is not empty, all the data it contains will be lost.

*/
-- DropTable
PRAGMA foreign_keys=off;
DROP TABLE "OneTimeCode";
PRAGMA foreign_keys=on;

-- CreateTable
CREATE TABLE "SingleUseAuthTicket" (
    "code" TEXT NOT NULL PRIMARY KEY,
    "authId" TEXT NOT NULL,
    "loginScheme" TEXT NOT NULL,
    "expiresAt" DATETIME NOT NULL,
    "usedAt" DATETIME
);
