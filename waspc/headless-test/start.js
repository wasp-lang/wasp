const cp = require('child_process');
const readline = require('linebyline');

function spawn(name, cmd, args, done) {
  const spawnOptions = {
    detached: true,
  };
  const proc = cp.spawn(cmd, args, spawnOptions);

  // We close stdin stream on the new process because otherwise the start-app
  // process hangs.
  // See https://github.com/wasp-lang/wasp/pull/1218#issuecomment-1599098272.
  proc.stdin.destroy();

  readline(proc.stdout).on('line', data => {
    console.log(`\x1b[0m\x1b[33m[${name}][out]\x1b[0m ${data}`);
  });
  readline(proc.stderr).on('line', data => {
    console.log(`\x1b[0m\x1b[33m[${name}][err]\x1b[0m ${data}`);
  });
  proc.on('exit', () => {
    done();
  });
}

let numDone = 0;
const cb = () => {
  if (numDone == 1) process.exit(0);
  numDone++;
}
spawn('app', 'npm', ['run', 'example-app:start-app'], cb);
spawn('db', 'npm', ['run', 'example-app:start-db'], cb)
