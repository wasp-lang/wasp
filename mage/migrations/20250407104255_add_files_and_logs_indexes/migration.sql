-- CreateIndex
CREATE INDEX "File_projectId_idx" ON "File"("projectId");

-- CreateIndex
CREATE INDEX "Log_projectId_createdAt_idx" ON "Log"("projectId", "createdAt");
