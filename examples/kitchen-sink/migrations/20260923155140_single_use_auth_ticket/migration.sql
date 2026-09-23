/*
  Warnings:

  - You are about to drop the `OneTimeCode` table. If the table is not empty, all the data it contains will be lost.

*/
-- DropTable
DROP TABLE "OneTimeCode";

-- CreateTable
CREATE TABLE "SingleUseAuthTicket" (
    "code" TEXT NOT NULL,
    "authId" TEXT NOT NULL,
    "loginScheme" TEXT NOT NULL,
    "expiresAt" TIMESTAMP(3) NOT NULL,
    "usedAt" TIMESTAMP(3),

    CONSTRAINT "SingleUseAuthTicket_pkey" PRIMARY KEY ("code")
);
