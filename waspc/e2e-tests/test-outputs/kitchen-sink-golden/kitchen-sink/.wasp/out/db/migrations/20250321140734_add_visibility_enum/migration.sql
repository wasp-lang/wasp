-- CreateEnum
CREATE TYPE "TaskVisibility" AS ENUM ('PRIVATE', 'LINK_ONLY', 'PUBLIC');

-- AlterTable
ALTER TABLE "Task" ADD COLUMN     "visibility" "TaskVisibility" NOT NULL DEFAULT 'PRIVATE';
