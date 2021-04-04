{{={= =}=}}
import _ from 'lodash'

const env = process.env.NODE_ENV || 'development'

// TODO:
//   - Use dotenv library to consume env vars from a file.
//   - Use convict library to define schema and validate env vars.
//  https://codingsans.com/blog/node-config-best-practices

const defaultOrigins = [
    'localhost',
    'https://localhost:3000',
    'http://localhost:3000'
];

/**
 * @method
 * @summary Parses and returns all allowed origins.
 * @description All allowed origins for CORS settings are stored in an ORIGINS environment variable.
 *              Each origin is delimited by a comma.
 * @returns {Array<string>}
 */
const parseAllowedOrigins = () => {
  const { ORIGINS: origins } = process.env;
  return [
      ...origins.split(','),
      ...defaultOrigins
  ];
}

const config = {
  all: {
    env,
    port: parseInt(process.env.PORT) || 3001,
    origins: parseAllowedOrigins(),
    {=# isAuthEnabled =}
    auth: {
      jwtSecret: undefined
    }
    {=/ isAuthEnabled =}
  },
  development: {
    {=# isAuthEnabled =}
    auth: {
      jwtSecret: 'DEVJWTSECRET'
    }
    {=/ isAuthEnabled =}
  },
  production: {
    {=# isAuthEnabled =}
    auth: {
      jwtSecret: process.env.JWT_SECRET
    }
    {=/ isAuthEnabled =}
  }
}

console.log('config: ', config.all);

const resolvedConfig = _.merge(config.all, config[env])
export default resolvedConfig
