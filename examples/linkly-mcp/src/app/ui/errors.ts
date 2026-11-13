/** Extracts a readable message from a Wasp operation error. */
export function errorMessage(e: unknown): string {
  if (e && typeof e === "object") {
    const data = (e as { data?: { message?: string } }).data;
    if (data?.message) return data.message;
    const message = (e as { message?: string }).message;
    if (message) return message;
  }
  return "Something went wrong.";
}
