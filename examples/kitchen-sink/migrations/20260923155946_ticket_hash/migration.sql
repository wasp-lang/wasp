/*
  Warnings:

  - The primary key for the `SingleUseAuthTicket` table will be changed. If it partially fails, the table could be left without primary key constraint.
  - You are about to drop the column `code` on the `SingleUseAuthTicket` table. All the data in the column will be lost.
  - Added the required column `ticketHash` to the `SingleUseAuthTicket` table without a default value. This is not possible if the table is not empty.

*/
-- AlterTable
ALTER TABLE "SingleUseAuthTicket" DROP CONSTRAINT "SingleUseAuthTicket_pkey",
DROP COLUMN "code",
ADD COLUMN     "ticketHash" TEXT NOT NULL,
ADD CONSTRAINT "SingleUseAuthTicket_pkey" PRIMARY KEY ("ticketHash");
