import { Request, Response } from '../types'
import { FooBarContext } from '../apis/types'
export function fooBar(req: Request, res: Response, context: FooBarContext) {
  res.set('Access-Control-Allow-Origin', '*')
  res.json({ msg: 'Hello, world!' })
}

