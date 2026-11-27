import { execSync } from "node:child_process";

// Brings the test database schema up to date before any test runs.
export default function setup() {
  const url =
    process.env.TEST_DATABASE_URL ??
    "postgresql://postgresWaspDevUser:postgresWaspDevPass@localhost:5432/linkly_mcp_test";
  execSync("npx prisma migrate deploy --schema schema.prisma", {
    stdio: "pipe",
    env: { ...process.env, DATABASE_URL: url },
  });
}
