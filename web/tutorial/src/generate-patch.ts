import { $ } from 'zx'
import readline from 'readline'

$.verbose = true

console.log('Committing current state...')
await $`cd ./TodoApp && git add . && git commit -m "checkpoint"`

console.log('\n==============================================')
console.log('Now make your changes to the TodoApp project.')
console.log('When finished, return here and press Enter to generate the patch.')
console.log('==============================================\n')

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
})

await new Promise<void>((resolve) => {
  rl.question('Press Enter when ready to generate the patch...', () => {
    rl.close()
    resolve()
  })
})

console.log('\n==============================================')

await $`cd ./TodoApp && git diff`

console.log('\n==============================================')
