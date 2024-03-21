import {
  defineUserSignupFields,
  ensurePasswordIsPresent,
  ensureValidPassword,
  ensureValidUsername,
  sanitizeAndSerializeProviderData,
} from "wasp/server/auth";

import { type CustomSignup } from "wasp/server/operations";
import { prisma } from "wasp/server";
import * as z from 'zod'

export const userSignupFields = defineUserSignupFields({
  address: (data) => {
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
