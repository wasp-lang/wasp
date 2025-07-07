export function generateBrightColor(
  hue = Math.floor(Math.random() * 360),
): string {
  const saturation = 90;
  const lightness = 80;

  return `hsl(${hue}, ${saturation}%, ${lightness}%)`;
}
