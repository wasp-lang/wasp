import { FooBar } from '@wasp/apis/types'

export const fooBar: FooBar = (req, res, context) => {
  res.set('Access-Control-Allow-Origin', '*') // Example of modifying headers to override Wasp default CORS middleware.
  res.json({ msg: `Hello, ${context.user?.email || 'stranger'}!` })
}
