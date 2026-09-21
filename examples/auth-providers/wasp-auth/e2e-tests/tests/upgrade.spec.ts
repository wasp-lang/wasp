import { expect, test } from "@playwright/test";
import { DatabaseSync } from "node:sqlite";
import path from "node:path";

/**
 * An app upgraded from a Wasp that had only its built-in auth. Its identity
 * rows were written before the `handlerName` column and before secrets had a
 * column of their own, so the password hash sits in `providerData`. The
 * column's default puts such rows under the `wasp` handler, and the first
 * login moves the hash to `providerSecrets`. No data migration is needed.
 */

const databasePath = path.resolve(
  import.meta.dirname,
  "../../.wasp/out/db/dev.db",
);
const uniqueSuffix = `${Date.now()}-${Math.floor(Math.random() * 10000)}`;
const username = `erin-${uniqueSuffix}`;
const password = "password1234";

test.describe.configure({ mode: "serial" });

function readIdentity(db: DatabaseSync) {
  return db
    .prepare(
      `SELECT "handlerName", "providerName", "providerData", "providerSecrets"
         FROM "AuthIdentity" WHERE "providerUserId" = ?`,
    )
    .get(username) as {
    handlerName: string;
    providerName: string;
    providerData: string;
    providerSecrets: string;
  };
}

test("an identity is recorded under its handler and its provider name", async ({
  request,
}) => {
  const signup = await request.post("/auth/wasp/username/signup", {
    data: { username, password },
  });
  expect(signup.status()).toBe(200);

  const db = new DatabaseSync(databasePath);
  const identity = readIdentity(db);
  db.close();
  expect(identity.handlerName).toBe("wasp");
  expect(identity.providerName).toBe("username");
  expect(JSON.parse(identity.providerData).hashedPassword).toBeUndefined();
  expect(typeof JSON.parse(identity.providerSecrets).hashedPassword).toBe(
    "string",
  );
});

test("a row from before the upgrade logs in, and its hash moves to the secrets", async ({
  request,
}) => {
  // Rewrite the row the way the old Wasp stored it: the hash in
  // `providerData`, no secrets.
  const db = new DatabaseSync(databasePath);
  const { providerSecrets } = readIdentity(db);
  const { hashedPassword } = JSON.parse(providerSecrets);
  db.prepare(
    `UPDATE "AuthIdentity" SET "providerData" = ?, "providerSecrets" = '{}'
       WHERE "providerUserId" = ?`,
  ).run(JSON.stringify({ hashedPassword }), username);

  const login = await request.post("/auth/wasp/username/login", {
    data: { username, password },
  });
  expect(login.status()).toBe(200);

  const migrated = readIdentity(db);
  db.close();
  expect(JSON.parse(migrated.providerSecrets).hashedPassword).toBe(
    hashedPassword,
  );
  expect(JSON.parse(migrated.providerData).hashedPassword).toBeNull();

  // And the second login reads it from where it now lives.
  const again = await request.post("/auth/wasp/username/login", {
    data: { username, password },
  });
  expect(again.status()).toBe(200);
});

test("a wrong password still fails for a row from before the upgrade", async ({
  request,
}) => {
  const db = new DatabaseSync(databasePath);
  const { providerSecrets } = readIdentity(db);
  db.prepare(
    `UPDATE "AuthIdentity" SET "providerData" = ?, "providerSecrets" = '{}'
       WHERE "providerUserId" = ?`,
  ).run(
    JSON.stringify({
      hashedPassword: JSON.parse(providerSecrets).hashedPassword,
    }),
    username,
  );
  db.close();

  const login = await request.post("/auth/wasp/username/login", {
    data: { username, password: "not-the-password1" },
  });
  expect(login.status()).toBe(401);
});
