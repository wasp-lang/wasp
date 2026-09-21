/*
  Warnings:

  - The primary key for the `AuthIdentity` table will be changed. If it partially fails, the table could be left without primary key constraint.

*/
-- RedefineTables
PRAGMA defer_foreign_keys=ON;
PRAGMA foreign_keys=OFF;
CREATE TABLE "new_AuthIdentity" (
    "handlerName" TEXT NOT NULL DEFAULT 'wasp',
    "providerName" TEXT NOT NULL,
    "providerUserId" TEXT NOT NULL,
    "providerClaims" TEXT NOT NULL DEFAULT '{}',
    "providerData" TEXT NOT NULL DEFAULT '{}',
    "providerSecrets" TEXT NOT NULL DEFAULT '{}',
    "authId" TEXT NOT NULL,

    PRIMARY KEY ("handlerName", "providerName", "providerUserId"),
    CONSTRAINT "AuthIdentity_authId_fkey" FOREIGN KEY ("authId") REFERENCES "Auth" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);
INSERT INTO "new_AuthIdentity" ("authId", "providerClaims", "providerData", "providerName", "providerSecrets", "providerUserId") SELECT "authId", "providerClaims", "providerData", "providerName", "providerSecrets", "providerUserId" FROM "AuthIdentity";
DROP TABLE "AuthIdentity";
ALTER TABLE "new_AuthIdentity" RENAME TO "AuthIdentity";
PRAGMA foreign_keys=ON;
PRAGMA defer_foreign_keys=OFF;
