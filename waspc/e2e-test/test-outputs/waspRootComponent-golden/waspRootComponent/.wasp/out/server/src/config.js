import _ from 'lodash'

const env = process.env.NODE_ENV || 'development'

// TODO:
//   - Use dotenv library to consume env vars from a file.
//   - Use convict library to define schema and validate env vars.
//  https://codingsans.com/blog/node-config-best-practices

const config = {
  all: {
    env,
    port: parseInt(process.env.PORT) || 3001,
    databaseUrl: process.env.DATABASE_URL,
    frontendUrl: undefined,
  },
  development: {
    frontendUrl: process.env.WASP_WEB_CLIENT_URL || 'http://localhost:3000',
  },
  production: {
    frontendUrl: process.env.WASP_WEB_CLIENT_URL,
  }
}

const resolvedConfig = _.merge(config.all, config[env])
export default resolvedConfig
