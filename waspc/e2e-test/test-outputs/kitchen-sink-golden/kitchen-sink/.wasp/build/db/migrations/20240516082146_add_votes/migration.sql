-- CreateTable
CREATE TABLE "TaskVote" (
    "id" TEXT NOT NULL,
    "userId" INTEGER NOT NULL,
    "taskId" INTEGER NOT NULL,

    CONSTRAINT "TaskVote_pkey" PRIMARY KEY ("id")
);

-- AddForeignKey
ALTER TABLE "TaskVote" ADD CONSTRAINT "TaskVote_userId_fkey" FOREIGN KEY ("userId") REFERENCES "User"("id") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "TaskVote" ADD CONSTRAINT "TaskVote_taskId_fkey" FOREIGN KEY ("taskId") REFERENCES "Task"("id") ON DELETE RESTRICT ON UPDATE CASCADE;
