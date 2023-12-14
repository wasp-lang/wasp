export function getNowInUTC() {
  const now = new Date();
  return new Date(now.toUTCString());
}
