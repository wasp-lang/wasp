import { testingAction } from 'wasp/client/operations'
import { sayHi } from '@util'

export default function setup() {
  console.log('This was called from the client setup function')
  testingAction()
  sayHi()
}
