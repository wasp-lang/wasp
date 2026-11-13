-- CreateTable
CREATE TABLE "McpRateLimit" (
    "id" TEXT NOT NULL,
    "windowStart" TIMESTAMP(3) NOT NULL,
    "count" INTEGER NOT NULL,

    CONSTRAINT "McpRateLimit_pkey" PRIMARY KEY ("id")
);

