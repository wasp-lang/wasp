export function fooBar(req: any, res: any, context: any) {
  console.log(context)
  res.set('Access-Control-Allow-Origin', '*')
  res.json({msg: "Hello, world!"})
}
