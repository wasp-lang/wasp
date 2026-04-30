export function getRailwayEnvVarValueReference(
  name: string,
  { serviceName }: { serviceName?: string } = {},
): string {
  // Railway variable references have the format ${{VARIABLE}} for local variables
  // or ${{serviceName.VARIABLE}} for cross-service references.
  // When the service name contains special characters (like hyphens with numbers),
  // Railway requires it to be quoted: ${{"service-name".VARIABLE}}

  const parts = [name];

  if (serviceName) {
    // JSON.stringify handles quoting for service names with special characters.
    parts.unshift(JSON.stringify(serviceName));
  }

  return "${{" + parts.join(".") + "}}";
}
