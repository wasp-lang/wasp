import { ReturnHelloJob } from '../../jobs/ReturnHelloJob'
export const returnHello: ReturnHelloJob<{ name: string }, string> = async (args) => {
  return args.name
}

