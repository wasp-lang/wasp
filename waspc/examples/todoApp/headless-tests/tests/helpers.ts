import type { Page } from '@playwright/test'

export async function performSignup(
  page: Page,
  { email, password }: { email: string; password: string }
) {
  await page.goto('/signup')

  await page.waitForSelector('text=Create a new account')

  await page.locator("input[type='email']").fill(email)
  await page.locator("input[type='password']").fill(password)
  await page.locator("input[name='address']").fill('Dummy address')
  await page.locator('button').click()
}

// We do this extra step to be able to run headless tests
// on production code that doesn't auto verify emails
export async function performEmailVerification(page: Page) {
  await page.goto('/headless-verify-emails')
  await page.waitForSelector('text=Verify all user emails')
  await page.locator('text=Verify all user emails').click()
  await page.waitForLoadState('networkidle')
}

export async function performLogin(
  page: Page,
  {
    email,
    password,
  }: {
    email: string
    password: string
  }
) {
  await page.goto('/login')

  await page.waitForSelector('text=Log in to your account')

  await page.locator("input[type='email']").fill(email)
  await page.locator("input[type='password']").fill(password)
  await page.getByRole('button', { name: 'Log in' }).click()
}

export function generateRandomCredentials(): {
  email: string
  password: string
} {
  return {
    email: `test${Math.random().toString(36).substring(7)}@test.com`,
    password: '12345678',
  }
}
