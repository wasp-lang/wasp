import { defineAdditionalSignupFields } from '@wasp/auth/index.js'
import * as z from 'zod'

export const fields = defineAdditionalSignupFields({
  address: (data) => {
    const AddressSchema = z
      .string({
        required_error: 'Address is required',
        invalid_type_error: 'Address must be a string',
      })
      .min(10, 'Address must be at least 10 characters long');
    const result = AddressSchema.safeParse(data.address);
    if (result.success === false) {
      throw new Error(result.error.issues[0].message);
    }
    return result.data;
  },
})