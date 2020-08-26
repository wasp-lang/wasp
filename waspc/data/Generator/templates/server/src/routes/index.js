{{={= =}=}}
import express from 'express'
import queries from './queries/index.js'

const router = express.Router()

router.get('/', function (req, res, next) {
  res.json('Hello world')
})

router.use('/{= queriesRouteInRootRouter =}', queries)

export default router
