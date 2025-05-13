import { test, expect } from '@playwright/test'
import {
  generateRandomCredentials,
  performLogin,
  performSignup,
} from './helpers'

test.describe('prisma setup fn', () => {
  const { email, password } = generateRandomCredentials()

  test.describe.configure({ mode: 'serial' })

  test('prisma setup hook hides a specific task', async ({ page }) => {
    await performSignup(page, {
      email,
      password,
    })

    await performLogin(page, {
      email,
      password,
    })

    await expect(page).toHaveURL('/profile')

    await page.goto('/')

    // Create a new task that will be hidden by the Prisma setup function
    const specificTask = 'hidden by setUpPrisma'
    await page.locator("input[type='text']").fill(specificTask)
    await page.getByText('Create new task').click()

    // Check that the save is submitted
    await expect(page.locator("input[type='text']")).toHaveValue('')

    await page.waitForLoadState('networkidle')

    const fullTaskText = `${specificTask} by ${email}`
    await expect(page.locator('body')).not.toHaveText(fullTaskText)
  })
})
