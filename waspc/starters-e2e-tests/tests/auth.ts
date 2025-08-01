import { expect, test, type Page } from "@playwright/test";
import { randomUUID } from "crypto";

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
  for (const [key, value] of Object.entries(credentials)) {
    await page.locator(`input[name='${key}']`).fill(String(value));
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
async function performEmailVerification(page: Page, sentToEmail: string) {
  if (isRunningInDevMode()) {
    return;
  }

  // Wait for the email to be sent
  await page.waitForTimeout(1000);

  const link = await getEmailVerificationLink(page, sentToEmail);

  await page.goto(link);
  await page.waitForSelector("text=Your email has been verified");
}

function isRunningInDevMode() {
  const testMode = process.env.HEADLESS_TEST_MODE ?? "dev";
  return testMode === "dev";
}

interface MailcrabMessage {
  id: string;
  to: { email: string }[];
}

interface MailcrabMessageDetails {
  text: string;
}

async function getEmailVerificationLink(
  page: Page,
  testUserEmail: string,
): Promise<string> {
  const mailcrabApiUrl = "http://localhost:1080";

  const mailcrabMessagesResponse = await page.request.get(
    `${mailcrabApiUrl}/api/messages`,
  );
  const mailcrabMessages =
    (await mailcrabMessagesResponse.json()) as MailcrabMessage[];

  const testUserMailcrabMessage = mailcrabMessages.find(
    (message) => message.to.at(0)?.email === testUserEmail,
  );
  if (!testUserMailcrabMessage) {
    throw new Error("No test user mailcrab message found");
  }

  const testUserMailcrabMessageDetailsResponse = await page.request.get(
    `${mailcrabApiUrl}/api/message/${testUserMailcrabMessage.id}`,
  );
  const testUserMailcrabMessageDetails =
    (await testUserMailcrabMessageDetailsResponse.json()) as MailcrabMessageDetails;

  const emailVerificationLinkMatch =
    testUserMailcrabMessageDetails.text.match(/https?:\/\/[^\s]+/);
  if (emailVerificationLinkMatch === null) {
    throw new Error("No email verification link found");
  }

  return emailVerificationLinkMatch[0];
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
