import { Request, Response } from '@wasp/types'
import { FooBarContext } from '@wasp/apis/types'

export function fooBar(req: Request, res: Response, context: FooBarContext) {
  res.set('Access-Control-Allow-Origin', '*') // Example of modifying headers to override CORS middleware.
  res.json({ msg: `Hello, ${context.user.username}!` })
}
