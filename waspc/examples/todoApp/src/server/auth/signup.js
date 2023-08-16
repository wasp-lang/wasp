export function getAdditionalFields() {
  return {
    address: {
      get: (value) => value,
      validate: (value) => {
        if (!value || value.length < 5) {
          throw new Error('Address is required')
        }
      },
    },
  }
}
