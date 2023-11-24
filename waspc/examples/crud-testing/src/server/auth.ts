import * as z from 'zod'
import { defineAdditionalSignupFields } from '@wasp/auth/index.js'
import {
  ensurePasswordIsPresent,
  ensureValidPassword,
  ensureValidUsername,
} from '@wasp/auth/validation.js'
import prisma from '@wasp/dbClient.js'

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
} as any)

import { CustomSignup } from '@wasp/actions/types'

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
        username: args.username,
        password: args.password,
        user: {
          create: {
            address: args.address,
          } as any,
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
