import { expect, test, type Page } from "@playwright/test";
import { generateRandomEmail, isRunningInDevMode } from "./helpers";
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
 When running the tests in **dev** mode, the email verification is done automatically.

 When running the tests in **build** mode, the email verification is required:
    * We rely on a Mailcrab SMTP dev server to receive emails locally when running the tests.
    * This is a local SMTP server that receives emails on port 1025 and exposes a REST API
    to fetch the emails. 
    * `wasp-app-runner` starts a Mailcrab server on port 1080 by default.
*/
export async function performEmailVerification(page: Page, email: string) {
  if (isRunningInDevMode()) {
    // This relies on having the SKIP_EMAIL_VERIFICATION_IN_DEV=true in the
    // .env.server file. This is the default value in the `.env.server.example` file.
    return;
  }

  // Wait for the email to be sent
  await page.waitForTimeout(1000);

  const link = await getMailCrabEmailVerificationLink(page, email);

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
  await page.locator("input[type='email']").fill(credentials.email);
  await page.locator("input[type='password']").fill(credentials.password);
  await page.getByRole("button", { name: "Log in" }).click();
}
