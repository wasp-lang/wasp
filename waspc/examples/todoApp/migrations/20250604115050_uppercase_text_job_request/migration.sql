-- CreateEnum
CREATE TYPE "UppercaseTextRequestState" AS ENUM ('PENDING', 'SUCCESS', 'ERROR');

-- CreateTable
CREATE TABLE "UppercaseTextRequest" (
    "id" TEXT NOT NULL,
    "input" TEXT NOT NULL,
    "output" TEXT,
    "userId" INTEGER NOT NULL,
    "state" "UppercaseTextRequestState" NOT NULL DEFAULT 'PENDING',
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,

    CONSTRAINT "UppercaseTextRequest_pkey" PRIMARY KEY ("id")
);

-- AddForeignKey
ALTER TABLE "UppercaseTextRequest" ADD CONSTRAINT "UppercaseTextRequest_userId_fkey" FOREIGN KEY ("userId") REFERENCES "User"("id") ON DELETE RESTRICT ON UPDATE CASCADE;
