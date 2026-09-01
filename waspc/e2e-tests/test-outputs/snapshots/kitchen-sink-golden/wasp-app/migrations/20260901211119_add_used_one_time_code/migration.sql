-- CreateTable
CREATE TABLE "UsedOneTimeCode" (
    "code" TEXT NOT NULL,
    "usedAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,

    CONSTRAINT "UsedOneTimeCode_pkey" PRIMARY KEY ("code")
);
