-- CreateTable
CREATE TABLE "Submission" (
    "name" TEXT NOT NULL,
    "email" TEXT NOT NULL,
    "github" TEXT NOT NULL,
    "description" TEXT NOT NULL,
    "twitter" TEXT,
    "country" TEXT,
    "website" TEXT,
    "image" TEXT,
    "approved" BOOLEAN NOT NULL DEFAULT false,
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,

    CONSTRAINT "Submission_pkey" PRIMARY KEY ("name")
);

-- CreateIndex
CREATE UNIQUE INDEX "Submission_name_key" ON "Submission"("name");

-- CreateIndex
CREATE UNIQUE INDEX "Submission_email_key" ON "Submission"("email");
