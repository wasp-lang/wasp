export function config() {
  console.log('Inside user-supplied Google config')
  return {
    scopes: ['profile', 'email'],
  }
}

export const userSignupFields = {}
