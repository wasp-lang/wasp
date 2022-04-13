import { sleep } from '@wasp/utils.js'

export const foo = async (args) => {
  console.log("Inside Job bar's callback foo: ", args)
  await sleep(4000)
  return { hello: "world" }
}
