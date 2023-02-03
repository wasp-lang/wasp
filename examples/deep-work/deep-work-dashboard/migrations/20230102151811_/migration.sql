-- CreateTable
CREATE TABLE "Work" (
    "id" SERIAL NOT NULL,
    "username" TEXT NOT NULL,
    "timeStarted" INTEGER NOT NULL,
    "minutes" TEXT NOT NULL,
    "userId" TEXT NOT NULL,

    CONSTRAINT "Work_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "User" (
    "id" SERIAL NOT NULL,
    "userId" TEXT NOT NULL,
    "username" TEXT NOT NULL,
    "password" TEXT NOT NULL,

    CONSTRAINT "User_pkey" PRIMARY KEY ("id")
);

-- CreateIndex
CREATE UNIQUE INDEX "User_userId_key" ON "User"("userId");

-- CreateIndex
CREATE UNIQUE INDEX "User_username_key" ON "User"("username");
