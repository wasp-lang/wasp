import {
  Bar,
  Customized,
  BarChart as RechartsBarChart,
  Rectangle,
  ResponsiveContainer,
  XAxis,
} from "recharts";

const renderCustomLabel = ({ x, y, width, value }) => {
  return value > 0 ? (
    <text
      x={x + width / 2}
      y={y}
      fill="black"
      textAnchor="middle"
      dy={-5}
      fontSize={12}
      style={{ fontFamily: "arial" }}
    >
      {value}
    </text>
  ) : null;
};

const CustomBackground = (props) => {
  const { width, height } = props;
  return (
    <Rectangle
      x={0}
      y={0}
      width={width}
      height={height}
      fill="rgb(241, 245, 249)"
      radius={14}
    />
  );
};

export function BarChart({ data }) {
  return (
    <ResponsiveContainer width="100%" height="100%">
      <RechartsBarChart
        data={data}
        margin={{ top: 25, right: 0, left: 0, bottom: 0 }}
      >
        <Customized component={CustomBackground} />
        <XAxis
          dataKey="displayValue"
          tick={{ fontSize: 11, fill: "#333" }}
          axisLine={true}
          tickLine={true}
        />
        <Bar
          dataKey="count"
          fill="#f9a8d4"
          label={renderCustomLabel}
          isAnimationActive={false}
        />
      </RechartsBarChart>
    </ResponsiveContainer>
  );
}
