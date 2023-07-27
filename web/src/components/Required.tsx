import * as React from "react";

const color = "#f59e0b";

export function Required() {
  return (
    <span
      style={{
        border: `2px solid ${color}`,
        display: "inline-block",
        padding: "0.2em 0.4em",
        color: color,
        borderRadius: "0.4em",
        fontSize: "0.8em",
        lineHeight: "1",
        fontWeight: "bold",
      }}
    >
      required
    </span>
  );
}
