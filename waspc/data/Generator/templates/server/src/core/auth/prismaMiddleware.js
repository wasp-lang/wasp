{{={= =}=}}
import { hashPassword } from '../auth.js'
import AuthError from '../AuthError.js'

const USERNAME_FIELD = 'username'
const PASSWORD_FIELD = 'password'

// Allows flexible validation of a user entity.
// Users can skip default validations by passing _waspSkipDefaultValidations = true
// Users can also add custom validations by passing an array of _waspCustomValidations
// with the same format as our default validations.
// Throws an AuthError on the first validation that fails.
const registerUserEntityValidation = (prismaClient) => {
  prismaClient.$use(async (params, next) => {
    if (params.model === '{= userEntityUpper =}') {
      if (['create', 'update', 'updateMany'].includes(params.action)) {
        validateUser(params.args.data, params.args, params.action)
      } else if (params.action === 'upsert') {
        validateUser(params.args.create, params.args, 'create')
        validateUser(params.args.update, params.args, 'update')
      }

      // Remove from downstream Prisma processing to avoid "Unknown arg" error
      delete params.args._waspSkipDefaultValidations
      delete params.args._waspCustomValidations
    }

    return next(params)
  })
}

// Make sure password is always hashed before storing to the database.
const registerPasswordHashing = (prismaClient) => {
  prismaClient.$use(async (params, next) => {
    if (params.model === '{= userEntityUpper =}') {
      if (['create', 'update', 'updateMany'].includes(params.action)) {
        if (params.args.data.hasOwnProperty(PASSWORD_FIELD)) {
          params.args.data[PASSWORD_FIELD] = await hashPassword(params.args.data[PASSWORD_FIELD])
        }
      } else if (params.action === 'upsert') {
        if (params.args.create.hasOwnProperty(PASSWORD_FIELD)) {
          params.args.create[PASSWORD_FIELD] =
            await hashPassword(params.args.create[PASSWORD_FIELD])
        }
        if (params.args.update.hasOwnProperty(PASSWORD_FIELD)) {
          params.args.update[PASSWORD_FIELD] =
            await hashPassword(params.args.update[PASSWORD_FIELD])
        }
      }
    }

    return next(params)
  })
}

export const registerAuthMiddleware = (prismaClient) => {
  // NOTE: registerUserEntityValidation must come before registerPasswordHashing.
  registerUserEntityValidation(prismaClient)
  registerPasswordHashing(prismaClient)
}
{=# isPasswordRequired =}

const passwordValidations = [
  { validates: USERNAME_FIELD, message: 'username must be present', validator: username => !!username },
  { validates: PASSWORD_FIELD, message: 'password must be present', validator: password => !!password },
  { validates: PASSWORD_FIELD, message: 'password must be at least 8 characters', validator: password => password.length >= 8 },
  { validates: PASSWORD_FIELD, message: 'password must contain a number', validator: password => /\d/.test(password) },
]
{=/ isPasswordRequired =}

const validateUser = (user, args, action) => {
  user = user || {}

  {=# isPasswordRequired =}
  const defaultValidations = [...passwordValidations]
  {=/ isPasswordRequired =}
  {=^ isPasswordRequired =}
  const defaultValidations = []
  {=/ isPasswordRequired =}

  const validations = [
    ...(args._waspSkipDefaultValidations ? [] : defaultValidations),
    ...(args._waspCustomValidations || [])
  ]

  // On 'create' validations run always, otherwise (on updates)
  // they run only when the field they are validating is present.
  for (const v of validations) {
    if (action === 'create' || user.hasOwnProperty(v.validates)) {
      if (!v.validator(user[v.validates])) {
        throw new AuthError(v.message)
      }
    }
  }
}
