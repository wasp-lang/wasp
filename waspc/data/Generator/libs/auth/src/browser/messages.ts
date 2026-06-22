export type ErrorMessage = {
  title: string;
  description?: string;
};

export function getDefaultErrorMessage(error: unknown): ErrorMessage {
  if (error instanceof Error) {
    const title = error.message || "Something went wrong";
    const description = isRecord(error) ? getWaspErrorDescription(error) : null;

    return description ? { title, description } : { title };
  }

  if (isRecord(error)) {
    const title = getString(error.message) ?? "Something went wrong";
    const description = getWaspErrorDescription(error);

    return description ? { title, description } : { title };
  }

  if (typeof error === "string" && error.length > 0) {
    return { title: error };
  }

  return { title: "Something went wrong" };
}

function getWaspErrorDescription(
  error: Record<string, unknown>,
): string | null {
  const data = error.data;
  if (!isRecord(data)) {
    return null;
  }

  const nestedData = data.data;
  if (!isRecord(nestedData)) {
    return null;
  }

  return getString(nestedData.message);
}

function getString(value: unknown): string | null {
  return typeof value === "string" && value.length > 0 ? value : null;
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null;
}
