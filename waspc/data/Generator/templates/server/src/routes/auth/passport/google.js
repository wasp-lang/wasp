{{={= =}=}}
import express from 'express'
import passport from 'passport'
import GoogleStrategy from 'passport-google-oauth2'
import { v4 as uuidv4 } from 'uuid'

import prisma from '../../../dbClient.js'
import waspServerConfig from '../../../config.js'
import { authConfig, googleFullRoutePrefix } from './config.js'

const router = express.Router()

// TODO: What if this name collides? For example, they import { config }
// and we imported config from '../../../config.js'?
{=& configJsFnImportStatement =}

const context = { entities: { {= userEntityUpper =}: prisma.{= userEntityLower =} } }

async function googleCallback(req, accessToken, refreshToken, profile, done) {
  console.log("In Google OAuth callback", accessToken, refreshToken, profile, done)

  // TODO: Make "google" a referenceable symbol.
  const user = await authConfig.onSignInFn("google", context, { profile })

  if (!user?.id) {
    throw new Error("auth.onSignInFn must return an object with an id property")
  }

  req.wasp = { ...req.wasp, userId: user.id }

  done(null, user)
}

// TODO: Verify we have what we need.
const userConfig = {= configJsFnIdentifier =}()

const callbackPath = '/oauth2/redirect'

passport.use(new GoogleStrategy.Strategy({
  clientID: userConfig.clientID,
  clientSecret: userConfig.clientSecret,
  callbackURL: `${googleFullRoutePrefix}${callbackPath}`,
  scope: ['email'],
  passReqToCallback: true
}, googleCallback))

// TODO: Add route helper for button in UI.
router.get('/login', passport.authenticate('google', { session: false }))

// Handle Google callback.
router.get(callbackPath,
  passport.authenticate('google', {
    session: false,
    failureRedirect: waspServerConfig.frontendUrl + authConfig.failureRedirectPath
  }),
  async function (req, res) {
    console.log("In Google success callback")
    const userId = req.wasp.userId

    if (!userId) {
      // NOTE: Should not happen if auth was successful.
      console.error('In passport success callback, but user not in request.')
      return res.redirect(`${waspServerConfig.frontendUrl}/login`)
    }

    const otpToken = await prisma.otpToken.create({ data: { userId, token: uuidv4() }})
    res.redirect(`${waspServerConfig.frontendUrl}/tokenExchange?otpToken=${otpToken.token}`)
  })

export default router
