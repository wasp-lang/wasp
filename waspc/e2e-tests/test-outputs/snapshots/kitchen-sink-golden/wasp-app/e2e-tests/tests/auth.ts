import { expect, test, type Page } from "@playwright/test";
import { getDevMailboxEmailVerificationLink } from "./devMailbox";
import {
  generateRandomEmail,
  isRunningInDeployedMode,
  isRunningInDevMode,
} from "./helpers";
import { getMailCrabEmailVerificationLink } from "./mailcrab";

interface BaseEmailCredentials {
  email: string;
  password: string;
}

interface UserSignupFields {
  [userSignupField: string]: unknown;
}

type Credentials = BaseEmailCredentials & UserSignupFields;

export function setupTestUser(): Credentials {
  const credentials: Credentials = {
    email: generateRandomEmail(),
    password: "12345678",
    address: "Some at least 10 letter address",
  };

  test.beforeAll(async ({ browser }) => {
    test.skip(
      isRunningInDeployedMode(),
      "Skipped in deployed mode (no Mailcrab)",
    );

    const page = await browser.newPage();
    await performSignup(page, credentials);
    await expect(page.locator("body")).toContainText(
      `You've signed up successfully! Check your email for the confirmation link.`,
    );

    await performEmailVerification(page, credentials.email);
    await page.close();
  });

  return credentials;
}

type SignupPage = "signup" | "manual-signup" | "custom-signup";

export async function performSignup(
  page: Page,
  credentials: Credentials,
  signupPage: SignupPage = "signup",
) {
  await navigateToSignupPage(page, signupPage);
  await submitSignupForm(page, credentials);
}

async function navigateToSignupPage(page: Page, signupPage: SignupPage) {
  await page.goto(`/${signupPage}`);
  await page.waitForSelector("text=Password");
}

async function submitSignupForm(page: Page, credentials: Credentials) {
  for (const [formFieldName, formFieldValue] of Object.entries(credentials)) {
    await page
      .locator(`input[name='${formFieldName}']`)
      .fill(String(formFieldValue));
  }
  await page.getByRole("button", { name: "Sign up" }).click();
}

/*
 When running the tests in **dev** mode, the app uses the Dummy email sender,
 so we read the email from the dev mailbox API.

 When running the tests in **build** mode, the app sends emails through SMTP:
    * We rely on a Mailcrab SMTP dev server to receive emails locally when running the tests.
    * This is a local SMTP server that receives emails on port 1025 and exposes a REST API
    to fetch the emails.
    * `wasp-app-runner` starts a Mailcrab server on port 1080 by default.
*/
export async function performEmailVerification(page: Page, email: string) {
  const link = isRunningInDevMode()
    ? await getDevMailboxEmailVerificationLink(page, email)
    : await getMailCrabEmailVerificationLinkAfterDelivery(page, email);

  await page.goto(link);
  await page.waitForSelector("text=Your email has been verified");
}

export async function performLogin(
  page: Page,
  credentials: BaseEmailCredentials,
) {
  await navigateToLoginPage(page);
  await submitLoginForm(page, credentials);
}

async function navigateToLoginPage(page: Page) {
  await page.goto("/login");
  await page.waitForSelector("text=Log in to your account");
}

async function submitLoginForm(page: Page, credentials: BaseEmailCredentials) {
  await page.locator("input[name='email']").fill(credentials.email);
  await page.locator("input[type='password']").fill(credentials.password);
  await page.getByRole("button", { name: "Log in" }).click();
}

async function getMailCrabEmailVerificationLinkAfterDelivery(
  page: Page,
  email: string,
): Promise<string> {
  // Wait for the email to be sent
  await page.waitForTimeout(1000);
  return getMailCrabEmailVerificationLink(page, email);
}
