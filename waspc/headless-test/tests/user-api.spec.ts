import { test, expect } from '@playwright/test'

test.describe('user API', () => {
  const randomEmail = `test${Math.random().toString(36).substring(7)}@test.com`
  const password = '12345678'

  test.describe.configure({ mode: 'serial' })

  test.beforeAll(async ({ browser }) => {
    const page = await browser.newPage()

    // Sign up
    await page.goto('/signup')

    await page.waitForSelector('text=Create a new account')

    await page.locator("input[type='email']").fill(randomEmail)
    await page.locator("input[type='password']").fill(password)
    await page.locator('button').click()
  })

  test('user API works on the client', async ({ page }) => {
    await page.goto('/login')

    await page.waitForSelector('text=Log in to your account')

    await page.locator("input[type='email']").fill(randomEmail)
    await page.locator("input[type='password']").fill(password)
    await page.getByRole('button', { name: 'Log in' }).click()

    await page.waitForSelector('text=Profile page')

    await expect(page.locator('body')).toContainText(
      `Hello ${randomEmail}! Your status is verfied`,
    )

    await expect(page.locator('a[href="/profile"]')).toContainText(randomEmail)
  })
})
