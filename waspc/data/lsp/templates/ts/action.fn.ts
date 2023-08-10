import { {{upperDeclName}} } from '@wasp/actions/types'

type {{upperDeclName}}Payload = void
type {{upperDeclName}}Result = void
{{#named?}}export {{/named?}}const {{name}}: {{upperDeclName}}<{{upperDeclName}}Payload, {{upperDeclName}}Result> = async (args, context) => {
  // Implementation goes here
}

{{#default?}}
export default {{name}}
{{/default?}}
