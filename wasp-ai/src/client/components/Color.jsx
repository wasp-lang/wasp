import tailwindColors from "tailwindcss/colors";

export function Color({ value }) {
  return (
    <div
      className="w-5 h-5 rounded-full"
      style={{
        backgroundColor: value,
      }}
    ></div>
  );
}

export const availableColors = Object.entries(tailwindColors)
  .map(([name, color]) => {
    return {
      name,
      color: color[500],
    };
  })
  .filter(
    (color) =>
      ![
        "black",
        "white",
        "transparent",
        "inherit",
        "current",
        "lightBlue",
        "warmGray",
        "trueGray",
        "coolGray",
        "blueGray",
        "gray",
        "neutral",
        "zinc",
      ].includes(color.name)
  );
