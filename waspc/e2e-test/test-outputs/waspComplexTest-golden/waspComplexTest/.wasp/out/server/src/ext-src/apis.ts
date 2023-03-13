import { FooBar, FooBaz } from '../apis/types'
export const fooBar: FooBar = (req, res, context) => {
  res.set('Access-Control-Allow-Origin', '*')
  res.json({ msg: 'Hello, context.user.username!' })
}
export const fooBaz: FooBaz = (req, res, context) => {
  res.json({ msg: 'Hello, stranger!' })
}

