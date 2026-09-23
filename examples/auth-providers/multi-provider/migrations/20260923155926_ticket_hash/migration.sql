/*
  Warnings:

  - The primary key for the `SingleUseAuthTicket` table will be changed. If it partially fails, the table could be left without primary key constraint.
  - You are about to drop the column `code` on the `SingleUseAuthTicket` table. All the data in the column will be lost.
  - Added the required column `ticketHash` to the `SingleUseAuthTicket` table without a default value. This is not possible if the table is not empty.

*/
-- RedefineTables
PRAGMA defer_foreign_keys=ON;
PRAGMA foreign_keys=OFF;
CREATE TABLE "new_SingleUseAuthTicket" (
    "ticketHash" TEXT NOT NULL PRIMARY KEY,
    "authId" TEXT NOT NULL,
    "loginScheme" TEXT NOT NULL,
    "expiresAt" DATETIME NOT NULL,
    "usedAt" DATETIME
);
INSERT INTO "new_SingleUseAuthTicket" ("authId", "expiresAt", "loginScheme", "usedAt") SELECT "authId", "expiresAt", "loginScheme", "usedAt" FROM "SingleUseAuthTicket";
DROP TABLE "SingleUseAuthTicket";
ALTER TABLE "new_SingleUseAuthTicket" RENAME TO "SingleUseAuthTicket";
PRAGMA foreign_keys=ON;
PRAGMA defer_foreign_keys=OFF;
