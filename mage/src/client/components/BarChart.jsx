import { AxisBottom } from "@visx/axis";
import { Group } from "@visx/group";
import { scaleBand, scaleLinear } from "@visx/scale";
import { Bar } from "@visx/shape";
import { useMemo } from "react";

const verticalMargin = 50;
const margins = {
  left: 0,
};

export function BarChart({ data, width, height }) {
  // bounds
  const xMax = width - margins.left;
  const yMax = height - verticalMargin;

  // scales, memoize for performance
  const xScale = useMemo(
    () =>
      scaleBand({
        range: [0, xMax],
        round: true,
        domain: data.map((bucket) => bucket.displayValue),
        padding: 0.4,
      }),
    [data, xMax],
  );
  const yScale = useMemo(
    () =>
      scaleLinear({
        range: [yMax, 0],
        round: true,
        domain: [0, Math.max(...data.map((bucket) => bucket.count))],
      }),
    [data, yMax],
  );

  return width < 10 ? null : (
    <svg width={width} height={height}>
      <rect width={width} height={height} className="fill-slate-100" rx={14} />
      <Group top={verticalMargin / 2} left={margins.left}>
        {data.map((d) => {
          const barWidth = xScale.bandwidth();
          const barHeight = yMax - (yScale(d.count) ?? 0);
          const barX = xScale(d.displayValue);
          const barY = yMax - barHeight;
          return (
            d.count > 0 && (
              <Group key={`bar-${d.date}`}>
                <Bar
                  x={barX}
                  y={barY}
                  width={barWidth}
                  height={barHeight}
                  className="fill-pink-300"
                />

                <text
                  x={barX + barWidth / 2}
                  y={yMax - barHeight}
                  fill="black"
                  fontSize={12}
                  dy={"-.33em"}
                  style={{ fontFamily: "arial", textAnchor: "middle" }}
                >
                  {d.count}
                </text>
              </Group>
            )
          );
        })}
        <AxisBottom
          numTicks={data.length}
          top={yMax}
          scale={xScale}
          tickLabelProps={() => ({
            fill: "#333",
            fontSize: 11,
            textAnchor: "middle",
          })}
        />
      </Group>
    </svg>
  );
}
