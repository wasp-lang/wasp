import { sleep } from "./sleep";

async function runBarJob(args, context) {
  console.log("Inside Job bar's callback foo: ", args, context);
  await sleep(4000);
  return { hello: "world" };
}

export async function mySpecialJob(args, context) {
  return runBarJob(args, context);
}

export async function mySpecialScheduledJob(args, context) {
  return runBarJob(args, context);
}
