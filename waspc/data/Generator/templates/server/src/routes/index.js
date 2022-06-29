{{={= =}=}}
import express from 'express'
import fs from 'fs'
import { fileURLToPath } from 'url'
import path, { dirname } from 'path'

import operations from './operations/index.js'
{=# isAuthEnabled =}
import auth from './auth/index.js'
{=/ isAuthEnabled =}


const router = express.Router()

router.get('/', function (req, res, next) {
  res.json('Hello world')
})

{=# isAuthEnabled =}
router.use('/auth', auth)

router.get('/session.html', function(req, res) {
  // TODO: Serve properly.
  const __filename = fileURLToPath(import.meta.url)
  const __dirname = dirname(__filename)
  const htmlTemplate = fs.readFileSync(path.resolve(__dirname, './session.html'), { encoding: 'utf8' })
  const html = htmlTemplate.replace("{{CSRF_TOKEN}}", req.csrfToken())
  res.send(html)
})
{=/ isAuthEnabled =}
router.use('/{= operationsRouteInRootRouter =}', operations)

export default router
