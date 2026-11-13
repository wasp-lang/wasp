{{={= =}=}}
import { makeOpenApiDocument } from '@wasp/lib-sdk-core/node'
import { config } from 'wasp/server'

export default makeOpenApiDocument({
  title: {=& title =},
  version: {=& version =},
  serverUrl: config.serverUrl,
  apis: [
    {=# apis =}
    {=& . =},
    {=/ apis =}
  ],
})
