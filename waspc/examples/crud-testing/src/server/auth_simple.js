import { defineUserFields } from '@wasp/auth/index.js'

export const getUserFields = () =>
  defineUserFields({
    address: (data) => data.address,
  })
