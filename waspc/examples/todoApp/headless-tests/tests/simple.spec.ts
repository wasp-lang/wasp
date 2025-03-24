import { test, expect } from '@playwright/test'
import {
  generateRandomCredentials,
  performLogin,
  performSignup,
} from './helpers'

test('has title', async ({ page }) => {
  await page.goto('/')

  await expect(page).toHaveTitle(/ToDo App/)
})
test.describe('signup and login', () => {
  const { email, password } = generateRandomCredentials()

  test.describe.configure({ mode: 'serial' })

  test('social button renders', async ({ page }) => {
    await page.goto('/signup')

    await page.waitForSelector('text=Create a new account')

    await expect(
      page.locator("a[href='http://localhost:3001/auth/google/login']")
    ).toBeVisible()
  })

  test('can sign up', async ({ page }) => {
    await performSignup(page, {
      email,
      password,
    })

    await expect(page.locator('body')).toContainText(
      `You've signed up successfully! Check your email for the confirmation link.`
    )
  })

  test('can log in and create a task', async ({ page }) => {
    await performLogin(page, {
      email,
      password: '12345678xxx',
    })

    await expect(page.locator('body')).toContainText('Invalid credentials')

    await performLogin(page, {
      email,
      password,
    })

    await expect(page).toHaveURL('/profile')

    await page.goto('/')

    // Create a new task
    const randomTask = `New Task ${Math.random().toString(36).substring(7)}`
    await page.locator("input[type='text']").fill(randomTask)
    await page.getByText('Create new task').click()

    const fullTaskText = `${randomTask} by ${email}`
    await page.waitForSelector(`text=${fullTaskText}`)

    await expect(page.locator('body')).toContainText(fullTaskText)

    // Navigate to task page
    await page.locator(`text=${fullTaskText}`).click()
    const taskPageText = `description: ${randomTask}`
    await page.waitForSelector(`text=${taskPageText}`)
  })
})
