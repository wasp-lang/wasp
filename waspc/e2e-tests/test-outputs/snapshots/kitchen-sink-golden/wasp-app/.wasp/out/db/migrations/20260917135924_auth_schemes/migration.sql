/*
  Warnings:

  - You are about to drop the column `providerId` on the `Session` table. All the data in the column will be lost.
  - You are about to drop the column `providerSessionId` on the `Session` table. All the data in the column will be lost.
  - Added the required column `signedInBy` to the `Session` table without a default value. This is not possible if the table is not empty.

*/
-- AlterTable
ALTER TABLE "Auth" ADD COLUMN     "credentialsInvalidatedAt" TIMESTAMP(3);

-- AlterTable
ALTER TABLE "Session" DROP COLUMN "providerId",
DROP COLUMN "providerSessionId",
ADD COLUMN     "signedInBy" TEXT NOT NULL;
