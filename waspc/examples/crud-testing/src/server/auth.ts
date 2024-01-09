import * as z from 'zod'
import { defineAdditionalSignupFields } from '@wasp/auth/index.js'
import {
  ensurePasswordIsPresent,
  ensureValidPassword,
  ensureValidUsername,
} from '@wasp/auth/validation.js'
import prisma from '@wasp/dbClient.js'
import { CustomSignup } from '@wasp/actions/types'
import { sanitizeAndSerializeProviderData } from '@wasp/auth/utils.js'

export const fields = defineAdditionalSignupFields({
  address: (data) => {
    console.log('Received data:', data)
    const AddressSchema = z
      .string({
        required_error: 'Address is required',
        invalid_type_error: 'Address must be a string',
      })
      .min(10, 'Address must be at least 10 characters long')
    const result = AddressSchema.safeParse(data.address)
    if (result.success === false) {
      throw new Error(result.error.issues[0].message)
    }
    return result.data
  },
})

type CustomSignupInput = {
  username: string
  password: string
  address: string
}
type CustomSignupOutput = {
  success: boolean
  message: string
}

export const signup: CustomSignup<
  CustomSignupInput,
  CustomSignupOutput
> = async (args) => {
  ensureValidUsername(args)
  ensurePasswordIsPresent(args)
  ensureValidPassword(args)

  try {
    await prisma.auth.create({
      data: {
        user: {
          create: {
            address: args.address,
          },
        },
        identities: {
          create: {
            providerName: 'username',
            providerUserId: args.username,
            providerData: await sanitizeAndSerializeProviderData<'username'>({
              hashedPassword: args.password,
            }),
          },
        },
      },
    })
  } catch (e: any) {
    return {
      success: false,
      message: e.message,
    }
  }

  return {
    success: true,
    message: 'User created successfully',
  }
}
