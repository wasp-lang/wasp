/*
  Warnings:

  - Changed the type of `timeStarted` on the `Work` table. No cast exists, the column would be dropped and recreated, which cannot be done if there is data, since the column is required.

*/
-- AlterTable
ALTER TABLE "Work" DROP COLUMN "timeStarted",
ADD COLUMN     "timeStarted" TIMESTAMP(3) NOT NULL;
