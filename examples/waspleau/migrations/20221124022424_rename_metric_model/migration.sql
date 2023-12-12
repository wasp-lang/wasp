/*
  Warnings:

  - You are about to drop the `Metric` table. If the table is not empty, all the data it contains will be lost.

*/
-- DropTable
DROP TABLE "Metric";

-- CreateTable
CREATE TABLE "Datum" (
    "id" SERIAL NOT NULL,
    "name" TEXT NOT NULL,
    "value" TEXT NOT NULL,
    "updatedAt" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "Datum_pkey" PRIMARY KEY ("id")
);

-- CreateIndex
CREATE UNIQUE INDEX "Datum_name_key" ON "Datum"("name");
