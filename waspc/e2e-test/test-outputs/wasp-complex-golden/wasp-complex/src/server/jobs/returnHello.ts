import { ReturnHelloJob } from 'wasp/server/jobs'
export const returnHello: ReturnHelloJob<{ name: string }, string> = async (args) => {
  return args.name
}

