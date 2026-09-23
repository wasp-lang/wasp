/*
  Warnings:

  - You are about to drop the column `signedInBy` on the `Session` table. All the data in the column will be lost.
  - Added the required column `loginScheme` to the `Session` table without a default value. This is not possible if the table is not empty.

*/
-- AlterTable
ALTER TABLE "Session" DROP COLUMN "signedInBy",
ADD COLUMN     "loginScheme" TEXT NOT NULL;
