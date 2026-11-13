-- DropForeignKey
ALTER TABLE "McpAccessToken" DROP CONSTRAINT "McpAccessToken_connectionId_fkey";

-- DropForeignKey
ALTER TABLE "McpRefreshToken" DROP CONSTRAINT "McpRefreshToken_connectionId_fkey";

-- AlterTable
ALTER TABLE "McpConnection" ADD COLUMN     "refreshGeneration" INTEGER NOT NULL DEFAULT 0;

-- AlterTable
ALTER TABLE "McpPendingAction" DROP COLUMN "userId";

-- DropTable
DROP TABLE "McpAccessToken";

-- DropTable
DROP TABLE "McpAuthCode";

-- DropTable
DROP TABLE "McpClient";

-- DropTable
DROP TABLE "McpRefreshToken";

-- AddForeignKey
ALTER TABLE "McpConnection" ADD CONSTRAINT "McpConnection_userId_fkey" FOREIGN KEY ("userId") REFERENCES "User"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "McpPendingAction" ADD CONSTRAINT "McpPendingAction_connectionId_fkey" FOREIGN KEY ("connectionId") REFERENCES "McpConnection"("id") ON DELETE CASCADE ON UPDATE CASCADE;


-- Scope lists were space-separated; the store now keeps JSON arrays.
UPDATE "McpConnection" SET
  "scopes" = array_to_json(string_to_array("scopes", ' '))::text,
  "requestedScopes" = array_to_json(string_to_array("requestedScopes", ' '))::text;
