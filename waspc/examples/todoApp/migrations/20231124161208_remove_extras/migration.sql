/*
  Warnings:

  - You are about to drop the column `email` on the `User` table. All the data in the column will be lost.
  - You are about to drop the column `emailVerificationSentAt` on the `User` table. All the data in the column will be lost.
  - You are about to drop the column `isEmailVerified` on the `User` table. All the data in the column will be lost.
  - You are about to drop the column `password` on the `User` table. All the data in the column will be lost.
  - You are about to drop the column `passwordResetSentAt` on the `User` table. All the data in the column will be lost.
  - You are about to drop the `SocialLogin` table. If the table is not empty, all the data it contains will be lost.

*/
-- DropForeignKey
ALTER TABLE "SocialLogin" DROP CONSTRAINT "SocialLogin_userId_fkey";

-- DropIndex
DROP INDEX "User_email_key";

-- AlterTable
ALTER TABLE "User" DROP COLUMN "email",
DROP COLUMN "emailVerificationSentAt",
DROP COLUMN "isEmailVerified",
DROP COLUMN "password",
DROP COLUMN "passwordResetSentAt";

-- DropTable
DROP TABLE "SocialLogin";
