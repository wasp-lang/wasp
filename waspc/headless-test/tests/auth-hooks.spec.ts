import { test, expect } from '@playwright/test'
import {
  generateRandomCredentials,
  performLogin,
  performSignup,
} from './helpers'

test.describe('auth hooks', () => {
  test.describe.configure({ mode: 'serial' })

  /*
    We set up the "before signup hook" to throw an error for a specific email address.
  */
  test('before signup hook works', async ({ page }) => {
    const emailThatThrowsError = 'notallowed@email.com'
    const password = '12345678'

    await performSignup(page, {
      email: emailThatThrowsError,
      password,
    })

    await expect(page.locator('body')).toContainText(
      'On Before Signup Hook disallows this email.',
    )
  })

  /*
    We set up the "after signup hook" to set a value in the user object.
  */
  test('after signup hook works', async ({ page }) => {
    const { email, password } = generateRandomCredentials()

    await performSignup(page, {
      email,
      password,
    })

    await performLogin(page, {
      email,
      password,
    })

    await expect(page).toHaveURL('/profile')

    await expect(page.locator('body')).toContainText(
      'Value of user.isOnAfterSignupHookCalled is true.',
    )
  })
})
