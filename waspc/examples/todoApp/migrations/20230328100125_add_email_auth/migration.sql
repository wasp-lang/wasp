/*
  Warnings:

  - Added the required column `isEmailVerified` to the `User` table without a default value. This is not possible if the table is not empty.

*/
-- AlterTable
ALTER TABLE "User" ADD COLUMN     "isEmailVerified" BOOLEAN NOT NULL DEFAULT false,
ADD COLUMN     "emailVerificationSentAt" TIMESTAMP(3),
ADD COLUMN     "passwordResetSentAt" TIMESTAMP(3);
