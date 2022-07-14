{{={= =}=}}
import express from 'express'
import passport from 'passport'
import GoogleStrategy from 'passport-google-oauth20'

import prisma from '../../../dbClient.js'
import waspServerConfig from '../../../config.js'
import { authConfig } from '../config.js'
import { sign } from '../../../core/auth.js'

{=# doesConfigFnExist =}
// TODO: What if this name collides? For example, they import { config }
// and we imported config from '../../../config.js'?
{=& configFnImportStatement =}
{=/ doesConfigFnExist =}

{=# doesOnSignInFnExist =}
{=& onSignInFnImportStatement =}
{=/ doesOnSignInFnExist =}
{=^ doesOnSignInFnExist =}
import { findOrCreateUserEntity } from '../../../core/auth.js'
{=/ doesOnSignInFnExist =}


{=# doesConfigFnExist =}
const userConfigFnExists = true
const configFn = {= configFnIdentifier =}
{=/ doesConfigFnExist =}
{=^ doesConfigFnExist =}
const userConfigFnExists = false
function configFn() {
  return {
    clientId: process.env['GOOGLE_CLIENT_ID'],
    clientSecret: process.env['GOOGLE_CLIENT_SECRET'],
  }
}
{=/ doesConfigFnExist =}

{=# doesOnSignInFnExist =}
const onSignInFn = {= onSignInFnIdentifier =}
{=/ doesOnSignInFnExist =}
{=^ doesOnSignInFnExist =}
async function onSignInFn (_context, args) {
  const email = args.profile.emails[0].value
  return await findOrCreateUserEntity(email)
}
{=/ doesOnSignInFnExist =}

const context = { entities: { {= userEntityUpper =}: prisma.{= userEntityLower =} } }

async function googleSuccessCallback(req, _accessToken, _refreshToken, profile, done) {
  try {
    const user = await onSignInFn(context, { profile })

    if (!user?.id) {
      return done(new Error('auth.onSignInFn must return a user object with an id property'))
    }

    req.wasp = { ...req.wasp, userId: user.id }

    done(null, user)
  } catch (err) {
    return done(err)
  }
}

const userConfig = validateConfig(configFn())

function validateConfig(config) {
  if (!config?.clientId) {
    if (userConfigFnExists) {
      throw new Error("auth.google.configFn must return an object with clientId property.")
    } else {
      throw new Error("Missing GOOGLE_CLIENT_ID environment variable.")
    }
  }

  if (!config?.clientSecret) {
    if (userConfigFnExists) {
      throw new Error("auth.google.configFn must return an object with clientSecret property.")
    } else {
      throw new Error("Missing GOOGLE_CLIENT_SECRET environment variable.")
    }
  }

  return config
}

passport.use(new GoogleStrategy({
  clientID: userConfig.clientId,
  clientSecret: userConfig.clientSecret,
  callbackURL: `${waspServerConfig.frontendUrl}/auth/redirect/google`,
  scope: [ 'email', 'profile' ],
  passReqToCallback: true
}, googleSuccessCallback))

const router = express.Router()

// Constructs a Google OAuth URL and redirects browser.
router.get('/login', passport.authenticate('google', { session: false }))

// Validates the OAuth code from the frontend, via server-to-server communication
// with Google. If valid, provides them with a JWT.
router.get('/validateCode',
  passport.authenticate('google', {
    session: false,
    failureRedirect: waspServerConfig.frontendUrl + authConfig.failureRedirectPath
  }),
  async function (req, res) {
    const userId = req.wasp.userId

    if (!userId) {
      console.error('In Google OAuth success callback, but userId not in request. This should not happen!')
      return res.status(500).send()
    }

    const token = await sign(userId)
    res.json({ token })
  })

export default router
