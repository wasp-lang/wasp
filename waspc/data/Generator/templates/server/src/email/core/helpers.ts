export function createFromEmailString({
    email,
    title,
  }: {
    email: string;
    title?: string;
  }): string {
    if (title) {
      return `${title} <${email}>`;
    }
    return email;
  }
