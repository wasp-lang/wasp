import { mySpecialJob } from '@wasp/jobs/mySpecialJob.js'

let someResource = undefined

export const getSomeResource = () => someResource

const setup = async () => {
  await new Promise(resolve => setTimeout(resolve, 2000))
  someResource = 'This resource is now set up.'
  console.log('Custom server setup done!')

  console.log('Kicking off Job...')
  // Or: const submittedJob = await mySpecialJob.delay(10).submit({ something: "here" })
  const submittedJob = await mySpecialJob.submit({ something: "here" })
  console.log(submittedJob.jobId, submittedJob.jobName, submittedJob.executorName)
  console.log("submittedJob.pgBoss.details()", await submittedJob.pgBoss.details())
}

export default setup

// WS test

import WebSocket, { WebSocketServer } from 'ws';
const ws = new WebSocketServer({ port: 8080 });

ws.on('connection', function connection(wsConnection) {
  wsConnection.on('message', function incoming(message) {
    console.log(`server received: ${message}`);
  });

  wsConnection.send('got your message!');
});
