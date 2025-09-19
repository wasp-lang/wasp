import { expect, test, type Page } from "@playwright/test";
import { randomUUID } from "crypto";
import { getMailCrabEmailVerificationLink } from "./mailcrab.js";

type Credentials = {
  email: string;
  password: string;
} & UserSignupFields;

type UserSignupFields = {
  [userSignupField: string]: unknown;
};

export function setupTestUser(
  userSignupFields?: UserSignupFields,
): Credentials {
  const credentials: Credentials = {
    ...userSignupFields,
    email: generateRandomEmail(),
    password: "12345678",
  };

  test.beforeAll(async ({ browser }) => {
    const page = await browser.newPage();
    await performSignup(page, credentials);
    await performEmailVerification(page, credentials.email);
    await page.close();
  });

  return credentials;
}

function generateRandomEmail(): string {
  return `${randomUUID()}@test.com`;
}

async function performSignup(page: Page, credentials: Credentials) {
  await page.goto("/signup");
  await page.waitForSelector("text=Create a new account");

  await submitSignupForm(page, credentials);

  await expect(page.locator("body")).toContainText(
    `You've signed up successfully! Check your email for the confirmation link.`,
  );
}

async function submitSignupForm(page: Page, credentials: Credentials) {
  for (const [formFieldName, formFieldValue] of Object.entries(credentials)) {
    await page
      .locator(`input[name='${formFieldName}']`)
      .fill(String(formFieldValue));
  }
  await page.getByRole("button", { name: "Sign up" }).click();
}

/**
  When running the tests in **dev** mode, we rely on `SKIP_EMAIL_VERIFICATION_IN_DEV=true`
  environment variable to ensure that the email verification is done automatically.

  When running the tests in **build** mode, we need to verify manually:
    * We rely on a Mailcrab SMTP dev server to receive emails locally when running the tests.
    * This is a local SMTP server that receives emails on port 1025 and exposes a REST API
    to fetch the emails. 
    * `wasp-app-runner` starts a Mailcrab server on port 1080 by default.
*/
async function performEmailVerification(page: Page, email: string) {
  if (isRunningInDevMode()) {
    return;
  }

  // Wait for the email to be sent
  await page.waitForTimeout(1000);

  const link = await getMailCrabEmailVerificationLink(page, email);

  await page.goto(link);
  await page.waitForSelector("text=Your email has been verified");
}

function isRunningInDevMode() {
  const testMode = process.env.WASP_RUN_MODE ?? "dev";
  return testMode === "dev";
}

export async function performLogin(page: Page, credentials: Credentials) {
  await page.goto("/login");
  await page.waitForSelector("text=Log in to your account");

  await submitLoginForm(page, credentials);

  await expect(page).toHaveURL("/");
}

async function submitLoginForm(page: Page, credentials: Credentials) {
  await page.locator("input[type='email']").fill(credentials.email);
  await page.locator("input[type='password']").fill(credentials.password);
  await page.getByRole("button", { name: "Log in" }).click();
}
