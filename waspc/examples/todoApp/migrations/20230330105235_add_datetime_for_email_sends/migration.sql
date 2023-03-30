-- AlterTable
ALTER TABLE "User" ADD COLUMN     "emailVerificationSentAt" TIMESTAMP(3),
ADD COLUMN     "passwordResetSentAt" TIMESTAMP(3);
