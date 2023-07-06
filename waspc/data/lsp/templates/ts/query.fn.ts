import { {{upperDeclName}} } from '@wasp/queries/types';

{{#named?}}export {{/named?}}const {{name}}: {{upperDeclName}}<void, void> = async (args, context) => {
  // Implementation goes here
}

{{#default?}}
export default {{name}}
{{/default?}}
