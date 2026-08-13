/*
  Warnings:

  - A unique constraint covering the columns `[authId,providerName]` on the table `AuthIdentity` will be added. If there are existing duplicate values, this will fail.

*/
-- CreateIndex
CREATE UNIQUE INDEX "AuthIdentity_authId_providerName_key" ON "AuthIdentity"("authId", "providerName");
