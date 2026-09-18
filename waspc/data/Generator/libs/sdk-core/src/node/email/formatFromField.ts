export function formatFromField({
  email,
  name,
}: {
  email: string;
  name?: string;
}): string {
  if (name) {
    return `${name} <${email}>`;
  }
  return email;
}
