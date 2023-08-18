import { {{upperDeclName}} } from '@wasp/actions/types'

type {{upperDeclName}}Input = void
type {{upperDeclName}}Output = void

{{#named?}}export {{/named?}}const {{name}}: {{upperDeclName}}<{{upperDeclName}}Input, {{upperDeclName}}Output> = async (args, context) => {
  // Implementation goes here
}

{{#default?}}
export default {{name}}
{{/default?}}
