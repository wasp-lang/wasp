{{#named?}}export {{/named?}}const {{name}} = async (args, context) => {
  // Implementation goes here
}

{{#default?}}
export default {{name}}
{{/default?}}

