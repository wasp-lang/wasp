const cp = require('child_process')
const readline = require('linebyline')

function spawn(name, cmd, args, done) {
  const spawnOptions = {
    detached: true,
    stdio: ['ignore', 'pipe', 'pipe'],
  }
  const proc = cp.spawn(cmd, args, spawnOptions)

  readline(proc.stdout).on('line', (data) => {
    console.log(`\x1b[0m\x1b[33m[${name}][out]\x1b[0m ${data}`)
  })
  readline(proc.stderr).on('line', (data) => {
    console.log(`\x1b[0m\x1b[33m[${name}][err]\x1b[0m ${data}`)
  })
  proc.on('exit', done)
}

// Exit if either child fails
const cb = (code) => {
  if (code !== 0) {
    process.exit(code)
  }
}
spawn('app', 'npm', ['run', 'headless:start-app'], cb)
spawn('db', 'npm', ['run', 'headless:start-db'], cb)
