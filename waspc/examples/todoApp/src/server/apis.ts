import { Request, Response } from '@wasp/types'

export function fooBar(req: Request, res: Response, context: any) {
  res.set('Access-Control-Allow-Origin', '*')
  res.json({msg: "Hello, world!"})
}
