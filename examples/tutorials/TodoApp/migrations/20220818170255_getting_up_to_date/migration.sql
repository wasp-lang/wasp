-- RedefineIndex
DROP INDEX "User.username_unique";
CREATE UNIQUE INDEX "User_username_key" ON "User"("username");
