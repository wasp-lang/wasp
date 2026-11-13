import { makeOpenApiDocument } from '@wasp/lib-sdk-core/node'
import { config } from 'wasp/server'

export default makeOpenApiDocument({
  title: "Wasp Kitchen Sink",
  version: "0.0.0",
  serverUrl: config.serverUrl,
  apis: [
    {"method":"ALL","name":"fooBar","path":"/foo/bar","usesAuth":true},
    {"method":"HEAD","name":"headBarBaz","path":"/bar/baz","usesAuth":false},
    {"method":"GET","name":"barBaz","path":"/bar/baz","usesAuth":false},
    {"method":"PATCH","name":"patchBarBaz","path":"/bar/baz","usesAuth":false},
    {"method":"POST","name":"webhookCallback","path":"/webhook/callback","usesAuth":false},
    {"method":"GET","name":"throwUnexpectedError","path":"/errors/unexpected","usesAuth":false},
    {"method":"GET","name":"throwHttpError","path":"/errors/http","usesAuth":false},
    {"method":"GET","name":"throwConcealedError","path":"/errors/concealed","usesAuth":false},
    {"method":"GET","name":"throwUnavailableError","path":"/errors/unavailable","usesAuth":false},
    {"method":"GET","name":"throwRateLimitError","path":"/errors/rate-limit","usesAuth":false},
    {"method":"GET","name":"throwUpstreamError","path":"/errors/upstream","usesAuth":false},
    {"method":"GET","name":"streamingText","path":"/api/streaming-test","usesAuth":true},
  ],
})
