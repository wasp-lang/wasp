/*
  Warnings:

  - You are about to drop the column `email` on the `Auth` table. All the data in the column will be lost.
  - You are about to drop the column `emailVerificationSentAt` on the `Auth` table. All the data in the column will be lost.
  - You are about to drop the column `isEmailVerified` on the `Auth` table. All the data in the column will be lost.
  - You are about to drop the column `password` on the `Auth` table. All the data in the column will be lost.
  - You are about to drop the column `passwordResetSentAt` on the `Auth` table. All the data in the column will be lost.
  - You are about to drop the column `username` on the `Auth` table. All the data in the column will be lost.

*/
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
