import { sayHi } from '../shared/util'

export default function setup() {
  console.log("This was called from the client setup function")
  sayHi()
}
