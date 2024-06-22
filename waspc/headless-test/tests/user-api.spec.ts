import { test, expect } from '@playwright/test'
import { generateRandomCredentials, performSignup } from './helpers'

test.describe('user API', () => {
  const { email, password } = generateRandomCredentials()

  test.describe.configure({ mode: 'serial' })

  test.beforeAll(async ({ browser }) => {
    const page = await browser.newPage()

    await performSignup(page, {
      email,
      password,
    })
  })

  test('user API works on the client', async ({ page }) => {
    await page.goto('/login')

    await page.waitForSelector('text=Log in to your account')

    await page.locator("input[type='email']").fill(email)
    await page.locator("input[type='password']").fill(password)
    await page.getByRole('button', { name: 'Log in' }).click()

    await page.waitForSelector('text=Profile page')

    await expect(page.locator('body')).toContainText(
      `Hello ${email}! Your status is verfied`,
    )

    await expect(page.locator('a[href="/profile"]')).toContainText(email)
  })
})
