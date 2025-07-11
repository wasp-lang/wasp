import { ColorRadioButton } from "./ColorRadioButton";
import { generateBrightColor } from "./colors";

interface ColorRadioButtonsProps {
  color: string;
  setColor: (color: string) => void;
}

export function ColorRadioButtons({ color, setColor }: ColorRadioButtonsProps) {
  const randomColor = generateBrightColor();

  return (
    <fieldset className="space-y-1">
      <legend className="label">Color</legend>
      <div className="mt-1 flex flex-wrap gap-2">
        <ColorRadioButton
          name="color"
          value={randomColor}
          checked={!staticColors.includes(color)}
          onChange={() => setColor(randomColor)}
          title="Random"
          bgColor={`conic-gradient(
              hsl(360 100% 50%),
              hsl(315 100% 50%),
              hsl(270 100% 50%),
              hsl(225 100% 50%),
              hsl(180 100% 50%),
              hsl(135 100% 50%),
              hsl(90 100% 50%),
              hsl(45 100% 50%),
              hsl(0 100% 50%)
            )`}
        />
        {staticColors.map((staticColor, index) => (
          <ColorRadioButton
            key={staticColor}
            name="color"
            value={staticColor}
            checked={color === staticColor}
            onChange={() => setColor(staticColor)}
            title={`Color ${index + 1}`}
            bgColor={staticColor}
          />
        ))}
      </div>
    </fieldset>
  );
}

const staticColors = generateBrightColors();

function generateBrightColors(): string[] {
  const colors: string[] = [];
  for (let hue = 0; hue < 360; hue += 20) {
    const hslColor = generateBrightColor(hue);
    colors.push(hslColor);
  }
  return colors;
}
