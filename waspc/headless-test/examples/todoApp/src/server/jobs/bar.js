export async function foo(args, context) {
  console.log("Inside Job bar's callback foo: ", args, context)
  await sleep(4000)
  return { hello: 'world' }
}

async function sleep(ms) {
  return new Promise((resolve) => {
    setTimeout(resolve, ms)
  })
}
