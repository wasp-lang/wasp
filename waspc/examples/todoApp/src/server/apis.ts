import { Request, Response } from '@wasp/types'
import { fooBarContext } from '@wasp/apis/types'

export function fooBar(req: Request, res: Response, context: fooBarContext) {
  res.set('Access-Control-Allow-Origin', '*')
  res.json({ msg: "Hello, world!" })
}
