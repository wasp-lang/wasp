-- AlterTable
ALTER TABLE "AuthIdentity" ADD COLUMN     "providerClaims" TEXT NOT NULL DEFAULT '{}',
ADD COLUMN     "providerSecrets" TEXT NOT NULL DEFAULT '{}';

-- AlterTable
ALTER TABLE "Session" ADD COLUMN     "providerSessionId" TEXT;
