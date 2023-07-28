{{#named?}}export {{/named?}}function {{name}}() {
  return (
    <div>Hello world!</div>
  )
}

{{#default?}}
export default {{name}}
{{/default?}}
