{{={= =}=}}
import express from 'express'

import prisma from '../../../dbClient.js'
import { sign } from '../../../core/auth.js'

{=# isGoogleAuthEnabled =}
import googleAuth from './google.js'
{=/ isGoogleAuthEnabled =}

const router = express.Router()

// This is a route that takes a one-time use, time-limited token
// to lookup what user just successfully logged in via Passport.
router.post('/otpTokenExchange', async (req, res) => {
  const args = req.body || {}
  const now = new Date()
  const minuteInMilliseconds = 60 * 1000

  const otpToken = await prisma.otpToken.findFirst({
    where: {
      token: args.otpToken,
      claimed: false,
      createdAt: {
        gte: new Date(now - minuteInMilliseconds),
        lte: now,
      },
    }
  })

  if (!otpToken) {
    return res.status(401).send()
  }

  await prisma.otpToken.update({ where: { id: otpToken.id }, data: { claimed: true } })
  const token = await sign(otpToken.userId)
  res.json({ token })
})

{=# isGoogleAuthEnabled =}
router.use('/google', googleAuth)
{=/ isGoogleAuthEnabled =}

export default router
