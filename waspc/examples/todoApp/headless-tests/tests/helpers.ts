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

/*
 When running the tests in **dev** mode, the email verification is done automatically.

 When running the tests in **build** mode, the email verification is required:
    * We rely on a Mailcrab SMTP dev server to receive emails locally when running the tests.
    * This is a local SMTP server that receives emails on port 1025 and exposes a REST API
    to fetch the emails. 
    * `wasp-app-runner` starts a Mailcrab server on port 1080 by default.
*/
export async function performEmailVerification(
  page: Page,
  sentToEmail: string
) {
  if (process.env.HEADLESS_TEST_MODE === 'dev') {
    // This relies on having the SKIP_EMAIL_VERIFICATION_IN_DEV=true in the
    // .env.server file. This is the default value in the .env.server.headless file.
    return
  }

  // Wait for the email to be sent
  await page.waitForTimeout(1000)

  const mailcrabApiUrl = 'http://localhost:1080'
  const messagesResponse = await page.request.get(
    `${mailcrabApiUrl}/api/messages`
  )
  const messages = (await messagesResponse.json()) as {
    id: string
    to: { email: string }[]
  }[]

  const message = messages.find(
    (message) => message.to[0].email === sentToEmail
  )
  if (!message) {
    throw new Error('No message found')
  }

  const messageDetailsResponse = await page.request.get(
    `${mailcrabApiUrl}/api/message/${message.id}`
  )
  const messageDetails = (await messageDetailsResponse.json()) as {
    text: string
  }
  const linkMatch = messageDetails.text.match(/https?:\/\/[^\s]+/)
  if (linkMatch === null) {
    throw new Error('No verification link found')
  }

  const link = linkMatch[0]
  await page.goto(link)
  await page.waitForSelector('text=Your email has been verified')
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
