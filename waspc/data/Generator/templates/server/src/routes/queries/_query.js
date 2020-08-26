{{={= =}=}}
import { handleRejection } from '../../utils.js'
{=& queryJsFnImportStatement =}

export default handleRejection(async (req, res) => {
  // TODO: We are letting default error handler handle errors, which returns all errors as 500.
  //   We should look into improving this, allowing users to return more information via errors.
  //   Important thing to think about is that not all errors from server can be forwarded to client
  //   because they could be exposing sensitive data, users should always be explicit about which errors
  //   can be exposed to the client, and all other errors should be 500.
  //   But let's first see in practice what we need from errors.

  // TODO: When generating express route for query, generated code would be most human-like if we
  //   generated GET route that uses query arguments.
  //   However, for that, we need to know the types of the arguments so we can cast/parse them.
  //   Also, there is limit on URI length, which could be problem if users want to send some bigger
  //   JSON objects or smth.
  //   So for now we are just going with POST that has JSON in the body -> generated code is not
  //   as human-like as it should be though.
  const result = await {= queryJsFnIdentifier =}({ args: req.body || {}, context: {} })
  res.json({ result })
})

