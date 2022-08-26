-- DropIndex
DROP INDEX "User.email_unique";

-- AlterTable
ALTER TABLE "User"
RENAME COLUMN "email" TO "username";

-- CreateIndex
CREATE UNIQUE INDEX "User.username_unique" ON "User"("username");
