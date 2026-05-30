export type Edit = {
  start: number;
  end: number;
  text: string;
};

export function applyEdits(sourceText: string, edits: Edit[]): string {
  const sortedEdits = [...edits].sort(
    (a, b) => a.start - b.start || a.end - b.end,
  );
  let sourceCursor = 0;
  let modifiedSource = "";

  for (const edit of sortedEdits) {
    modifiedSource += sourceText.slice(sourceCursor, edit.start);
    modifiedSource += edit.text;
    sourceCursor = edit.end;
  }

  modifiedSource += sourceText.slice(sourceCursor);

  return modifiedSource;
}
