import jobFactory from './PassthroughJobFactory.js'
import { foo } from './../ext-src/jobs/bar.js'

export default jobFactory(foo)
