import { defineUserSignupFields } from "wasp/server/auth";

export const userSignupFields = defineUserSignupFields({
  address: (data) => {
    if (typeof data.address !== 'string') {
      throw new Error('Address is required.')
    }
    if (data.address.length < 10) {
      throw new Error('Address must be at least 10 characters long.')
    }
    return data.address
  },
})
