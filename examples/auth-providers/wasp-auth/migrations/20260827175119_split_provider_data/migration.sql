-- RedefineTables
PRAGMA defer_foreign_keys=ON;
PRAGMA foreign_keys=OFF;
CREATE TABLE "new_AuthIdentity" (
    "providerName" TEXT NOT NULL,
    "providerUserId" TEXT NOT NULL,
    "providerClaims" TEXT NOT NULL DEFAULT '{}',
    "providerData" TEXT NOT NULL DEFAULT '{}',
    "providerSecrets" TEXT NOT NULL DEFAULT '{}',
    "authId" TEXT NOT NULL,

    PRIMARY KEY ("providerName", "providerUserId"),
    CONSTRAINT "AuthIdentity_authId_fkey" FOREIGN KEY ("authId") REFERENCES "Auth" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);
INSERT INTO "new_AuthIdentity" ("authId", "providerData", "providerName", "providerUserId") SELECT "authId", "providerData", "providerName", "providerUserId" FROM "AuthIdentity";
DROP TABLE "AuthIdentity";
ALTER TABLE "new_AuthIdentity" RENAME TO "AuthIdentity";
PRAGMA foreign_keys=ON;
PRAGMA defer_foreign_keys=OFF;
