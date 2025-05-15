import { test, expect } from '@playwright/test'
import { generateRandomCredentials, performLogin } from './helpers'

test.describe('custom signup', () => {
  const { email, password } = generateRandomCredentials()

  test.describe.configure({ mode: 'serial' })

  test('can sign up', async ({ page }) => {
    await page.goto('/custom-signup')

    await page.waitForSelector('text=Custom Signup Action')

    await page.locator("input[type='email']").fill(email)
    await page.locator("input[type='password']").fill(password)
    await page.locator("input[name='address']").fill('Dummy address')
    await page.locator('button[type="submit"]').click()

    await expect(page.locator('body')).toContainText(
      `Signup successful. You can now login.`
    )
  })

  test('can log in after signed up with custom action', async ({ page }) => {
    await performLogin(page, {
      email,
      password,
    })

    await expect(page).toHaveURL('/profile')
  })
})
