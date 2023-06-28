import { v4 as uuidv4 } from 'uuid'
import { spawn } from 'child_process'

// TODO: This will keep growing and growing, in memory forever. We need to clean up old results!
//   Or maybe let's even save it in the database?
const appGenerationResults = {}

export async function startGeneratingNewApp (args, _context) {
  const appId = uuidv4()

  appGenerationResults[appId] = {
    status: 'in-progress',
    messages: [],
    unconsumedStdout: ''
  }

  // TODO: Replace `new-ai` call with `new-ai:stdout` because it is more explicit.
  //   Then, also remove `new-ai` call from CLI, it will be redundant (cli/exe/Main.hs).
  let waspCliProcess = null
  if (process.env.NODE_ENV === 'production') {
    waspCliProcess = spawn('wasp', ['new-ai', args.appName, args.appDesc])
  } else {
    // NOTE: In dev when we use `wasp-cli`, we want to make sure that if this app is run via `wasp` that its datadir env var does not propagate,
    //   so we reset it here. This is problem only if you run app with `wasp` and let it call `wasp-cli` here.
    waspCliProcess = spawn('wasp-cli', ['new-ai', args.appName, args.appDesc], { env: { ...process.env, waspc_datadir: undefined}})
  }

  waspCliProcess.stdout.on('data', (data) => {
    console.log(data.toString())
    const newStdoutChunk = data.toString()
    let unconsumedStdout = appGenerationResults[appId].unconsumedStdout + newStdoutChunk
    let newMessages = []
    while (true) {
      const firstMsgEndHeaderMatch = unconsumedStdout.match(/===\/ WASP AI: (LOG|WRITE FILE) ====/)
      if (firstMsgEndHeaderMatch) {
        const msgEndHeaderStartIdx = firstMsgEndHeaderMatch.index
        const msgEndHeader = firstMsgEndHeaderMatch[0]
        const msgStartHeader = msgEndHeader.replace('===/', '====') + '\n'
        const msgStartHeaderMatch = unconsumedStdout.match(new RegExp(msgStartHeader))
        const msgStartHeaderStartIdx = msgStartHeaderMatch.index
        const message = {
          text: unconsumedStdout.substring(msgStartHeaderStartIdx + msgStartHeader.length, msgEndHeaderStartIdx),
          type: msgStartHeader === '==== WASP AI: LOG ====\n' ? 'log' : 'write-file'
        }
        newMessages = [ ...newMessages, message]
        unconsumedStdout = unconsumedStdout.substring(msgEndHeaderStartIdx + msgEndHeader.length)
      } else {
        break
      }
    }
    appGenerationResults[appId].messages = [ ...appGenerationResults[appId].messages, ...newMessages]
    appGenerationResults[appId].unconsumedStdout = unconsumedStdout
  })

  waspCliProcess.stderr.on('data', (data) => {
    console.error(data.toString())
  })

  waspCliProcess.on('close', (code) => {
    console.log('WASP CLI PROCESS STOPPED')
    if (code === 0) {
        appGenerationResults[appId].status = 'success'
    } else {
        appGenerationResults[appId].status = 'failure'
    }
  });

  return appId
}

export async function getAppGenerationResult (args, _context) {
  const appId = args.appId
  return {
    status: appGenerationResults[appId].status,
    messages: appGenerationResults[appId].messages
  }
}
