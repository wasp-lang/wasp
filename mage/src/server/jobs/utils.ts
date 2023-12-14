export function log(line: string): void {
  const date = formatDate(new Date());
  console.log(`[${date}] ${line}`);
}

function formatDate(date: Date): string {
  return `${date.getDate()}-${
    date.getMonth() + 1
  }-${date.getFullYear()} ${date.getHours()}:${date.getMinutes()}:${date.getSeconds()}`;
}
