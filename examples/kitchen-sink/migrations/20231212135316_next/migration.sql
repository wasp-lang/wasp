/*
  Warnings:

  - You are about to drop the column `email` on the `Auth` table. All the data in the column will be lost.
  - You are about to drop the column `emailVerificationSentAt` on the `Auth` table. All the data in the column will be lost.
  - You are about to drop the column `isEmailVerified` on the `Auth` table. All the data in the column will be lost.
  - You are about to drop the column `password` on the `Auth` table. All the data in the column will be lost.
  - You are about to drop the column `passwordResetSentAt` on the `Auth` table. All the data in the column will be lost.
  - You are about to drop the column `username` on the `Auth` table. All the data in the column will be lost.
  - You are about to drop the `SocialAuthProvider` table. If the table is not empty, all the data it contains will be lost.

*/
-- DropForeignKey
ALTER TABLE "SocialAuthProvider" DROP CONSTRAINT "SocialAuthProvider_authId_fkey";

-- DropIndex
DROP INDEX "Auth_email_key";

-- DropIndex
DROP INDEX "Auth_username_key";

-- AlterTable
ALTER TABLE "Auth" DROP COLUMN "email",
DROP COLUMN "emailVerificationSentAt",
DROP COLUMN "isEmailVerified",
DROP COLUMN "password",
DROP COLUMN "passwordResetSentAt",
DROP COLUMN "username";

-- DropTable
DROP TABLE "SocialAuthProvider";

-- CreateTable
CREATE TABLE "AuthIdentity" (
    "providerName" TEXT NOT NULL,
    "providerUserId" TEXT NOT NULL,
    "providerData" TEXT NOT NULL DEFAULT '{}',
    "authId" TEXT NOT NULL,

    CONSTRAINT "AuthIdentity_pkey" PRIMARY KEY ("providerName","providerUserId")
);

-- AddForeignKey
ALTER TABLE "AuthIdentity" ADD CONSTRAINT "AuthIdentity_authId_fkey" FOREIGN KEY ("authId") REFERENCES "Auth"("id") ON DELETE CASCADE ON UPDATE CASCADE;
