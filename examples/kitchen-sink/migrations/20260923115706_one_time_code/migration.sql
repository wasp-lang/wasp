-- CreateTable
CREATE TABLE "OneTimeCode" (
    "code" TEXT NOT NULL,
    "authId" TEXT NOT NULL,
    "signedInBy" TEXT NOT NULL,
    "expiresAt" TIMESTAMP(3) NOT NULL,
    "usedAt" TIMESTAMP(3),

    CONSTRAINT "OneTimeCode_pkey" PRIMARY KEY ("code")
);
