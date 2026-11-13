-- CreateTable
CREATE TABLE "McpPendingAction" (
    "id" TEXT NOT NULL,
    "connectionId" TEXT NOT NULL,
    "toolName" TEXT NOT NULL,
    "args" TEXT NOT NULL,
    "argsKey" TEXT NOT NULL,
    "summary" TEXT NOT NULL,
    "outcome" TEXT,
    "expiresAt" TIMESTAMP(3) NOT NULL,
    "approvedAt" TIMESTAMP(3),
    "deniedAt" TIMESTAMP(3),
    "reportedAt" TIMESTAMP(3),
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,

    CONSTRAINT "McpPendingAction_pkey" PRIMARY KEY ("id")
);

-- CreateIndex
CREATE INDEX "McpPendingAction_connectionId_toolName_argsKey_idx" ON "McpPendingAction"("connectionId", "toolName", "argsKey");

-- AddForeignKey
ALTER TABLE "McpPendingAction" ADD CONSTRAINT "McpPendingAction_connectionId_fkey" FOREIGN KEY ("connectionId") REFERENCES "McpConnection"("id") ON DELETE CASCADE ON UPDATE CASCADE;

