/*
  Warnings:

  - The primary key for the `AuthIdentity` table will be changed. If it partially fails, the table could be left without primary key constraint.

*/
-- AlterTable
ALTER TABLE "AuthIdentity" DROP CONSTRAINT "AuthIdentity_pkey",
ADD COLUMN     "handlerName" TEXT NOT NULL DEFAULT 'wasp',
ADD CONSTRAINT "AuthIdentity_pkey" PRIMARY KEY ("handlerName", "providerName", "providerUserId");
