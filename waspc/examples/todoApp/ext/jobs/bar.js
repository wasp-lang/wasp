function sleep(ms) {
  return new Promise((resolve) => {
    setTimeout(resolve, ms)
  })
}

export const foo = async (args) => {
  console.log("Inside Job bar's callback foo: ", args)
  await sleep(4000)
  return "I am the Job's result!"
}
