/*
  Warnings:

  - A unique constraint covering the columns `[name,userId]` on the table `Tag` will be added. If there are existing duplicate values, this will fail.

*/
-- DropIndex
DROP INDEX "Tag.name_unique";

-- CreateIndex
CREATE UNIQUE INDEX "Tag.name_userId_unique" ON "Tag"("name", "userId");
