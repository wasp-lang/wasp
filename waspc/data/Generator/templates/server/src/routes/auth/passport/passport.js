{{={= =}=}}
import express from 'express'

import prisma from '../../../dbClient.js'
import { sign } from '../../../core/auth.js'

{=# isGoogleAuthEnabled =}
import googleAuth from './google.js'
{=/ isGoogleAuthEnabled =}

const router = express.Router()

// This is a route that takes a 1-time use, time limited token
// to lookup what user just successfully logged in above.
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

  if (otpToken) {
    await prisma.otpToken.update({ where: { id: otpToken.id }, data: { claimed: true } })
    const token = await sign(otpToken.userId)
    return res.json({ token })
  }

  return res.status(401).send()
})

{=# isGoogleAuthEnabled =}
router.use('/google', googleAuth)
{=/ isGoogleAuthEnabled =}

export default router
