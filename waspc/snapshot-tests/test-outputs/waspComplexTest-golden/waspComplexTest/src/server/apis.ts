import { FooBar, FooBaz } from 'wasp/server/api'
import { MiddlewareConfigFn } from 'wasp/server'
export const fooBar: FooBar = (req, res, context) => {
  res.set('Access-Control-Allow-Origin', '*')
  res.json({ msg: 'Hello, context.user.username!' })
}
export const fooBaz: FooBaz = (req, res, context) => {
  res.json({ msg: 'Hello, stranger!' })
}
export const fooBarMiddlewareFn: MiddlewareConfigFn = (middlewareConfig) => {
  return middlewareConfig
}

