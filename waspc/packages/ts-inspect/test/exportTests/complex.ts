export default function isEvenOrOdd(n: number): boolean {
  return isEven(n) || isOdd(n);
}

export function isEven(n: number): boolean {
  if (n < 0) return isEven(-n);
  if (n == 0) return true;
  return isOdd(n - 1);
}

export function isOdd(n: number): boolean {
  if (n < 0) return isOdd(-n);
  if (n == 1) return true;
  return isEven(n - 1);
}
