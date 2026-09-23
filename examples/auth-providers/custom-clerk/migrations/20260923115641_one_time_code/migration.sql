-- CreateTable
CREATE TABLE "OneTimeCode" (
    "code" TEXT NOT NULL PRIMARY KEY,
    "authId" TEXT NOT NULL,
    "signedInBy" TEXT NOT NULL,
    "expiresAt" DATETIME NOT NULL,
    "usedAt" DATETIME
);
