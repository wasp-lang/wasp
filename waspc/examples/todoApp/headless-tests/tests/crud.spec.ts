import { expect, type Page, test } from '@playwright/test'
import {
  generateRandomCredentials,
  performLogin,
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

    await performEmailVerification(page, email)
  })

  test('crud list page', async ({ page }) => {
    await performLogin(page, {
      email,
      password,
    })

    await expect(page).toHaveURL('/profile')

    await page.goto('/crud')
    // wait for idle
    await page.waitForSelector('text=Tasks (CRUD feature)')

    // Create a task
    const taskDescription = 'first task'
    await createTask(page, taskDescription)

    await expect(page.locator('body')).toContainText(
      `${taskDescription} by ${email}`
    )

    // Edit the task
    await page.getByRole('button', { name: 'Edit' }).click()
    const editInput = page.getByTestId('edit-task-input')
    const newTaskDescription = 'edited task'

    await expect(editInput).toHaveValue(taskDescription)
    await editInput.fill(newTaskDescription)
    await page.getByRole('button', { name: 'Update task' }).click()
    await expect(page.locator('body')).toContainText(
      `${newTaskDescription} by ${email}`
    )

    // Delete the task
    page.on('dialog', async (dialog) => {
      expect(dialog.message()).toBe(
        'Are you sure you want to delete this task?'
      )
      await dialog.accept()
    })
    await page.locator('button').filter({ hasText: 'Delete' }).click()
    await page.waitForLoadState('networkidle')
    await expect(page.locator('body')).not.toContainText(
      `${newTaskDescription} by ${email}`
    )
    await expect(page.locator('body')).toContainText('No tasks yet.')
  })

  test('crud detail page', async ({ page }) => {
    await performLogin(page, {
      email,
      password,
    })

    await expect(page).toHaveURL('/profile')

    await page.goto('/crud')
    // Create a task
    await createTask(page, 'second task')
    // Go to the detail page of the task
    await page.locator('a').filter({ hasText: 'second task' }).click()
    await expect(page).toHaveURL(/\/crud\/\d+/)
    // Check if the task is displayed
    await expect(page.locator('body')).toContainText('second task')
  })
})

async function createTask(page: Page, description: string) {
  await page.getByRole('textbox').fill(description)
  await page.getByRole('button', { name: 'Create task' }).click()
}
