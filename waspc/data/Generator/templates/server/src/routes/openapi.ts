{{={= =}=}}
import { config } from 'wasp/server'

const openApiDocument = {=& openApiDocument =}

export default {
  ...openApiDocument,
  servers: [{ url: config.serverUrl }],
}
