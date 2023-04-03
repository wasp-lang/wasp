import { BarBaz, FooBar } from '@wasp/apis/types'

export const fooBar: FooBar = (_req, res, context) => {
  res.set('Access-Control-Allow-Origin', '*') // Example of modifying headers to override Wasp default CORS middleware.
  res.json({ msg: `Hello, ${context.user.username}!` })
}

export const barBaz: BarBaz = (_req, res, _context) => {
  res.set('Access-Control-Allow-Origin', '*')
  res.json({ msg: `Hello, stranger!` })
}
