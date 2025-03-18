-- AlterTable
ALTER TABLE "User" ADD COLUMN     "isOnAfterLoginHookCalled" BOOLEAN NOT NULL DEFAULT false,
ADD COLUMN     "isOnAfterSignupHookCalled" BOOLEAN NOT NULL DEFAULT false;
