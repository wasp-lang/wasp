// Shared settings for tests that run against a live `wasp start`.
export const TEST_DATABASE_URL =
  process.env.TEST_DATABASE_URL ??
  "postgresql://postgresWaspDevUser:postgresWaspDevPass@localhost:5432/linkly_test";
export const CLIENT_URL = "http://localhost:3000";
export const SERVER_URL = "http://localhost:3001";
