import { test, expect } from '@playwright/test'
import {
  generateRandomCredentials,
  performEmailVerification,
  performSignup,
} from './helpers'

test.describe('CRUD test', () => {
  const { email, password } = generateRandomCredentials()

  test.describe.configure({ mode: 'serial' })

  test.beforeAll(async ({ browser }) => {
    const page = await browser.newPage()

    await performSignup(page, {
      email,
      password,
    })

    await expect(page.locator('body')).toContainText(
      `You've signed up successfully! Check your email for the confirmation link.`
    )

    await performEmailVerification(page)
  })

  test('CRUD with override works', async ({ page }) => {
    await page.goto('/login')

    await page.waitForSelector('text=Log in to your account')

    await page.locator("input[type='email']").fill(email)
    await page.locator("input[type='password']").fill(password)
    await page.getByRole('button', { name: 'Log in' }).click()

    await page.waitForSelector('text=User Auth Fields Demo')

    await page.goto('/crud')

    await page.waitForSelector('text=Tasks')

    await createTask(page, 'special filter 1')
    await createTask(page, 'special filter 2')
    await createTask(page, 'something else')

    await expect(page.locator('body')).toContainText('special filter 1')
    await expect(page.locator('body')).toContainText('special filter 2')
    await expect(page.locator("li[text='something else']")).not.toBeVisible()
  })
})

async function createTask(page: any, description: string) {
  await page.locator("input[type='text']").fill(description)
  await page.getByText('Create task').click()
}
