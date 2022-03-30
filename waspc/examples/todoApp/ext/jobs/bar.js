function sleep(ms) {
  return new Promise((resolve) => {
    setTimeout(resolve, ms)
  })
}

export const foo = async (args) => {
  console.log("inside bar's foo")
  console.log(args)
  await sleep(4000)
  return 1
}
