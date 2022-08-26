-- CreateTable
CREATE TABLE "Excuse" (
    "id" INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    "text" TEXT NOT NULL
);

-- CreateIndex
CREATE UNIQUE INDEX "Excuse_text_key" ON "Excuse"("text");
