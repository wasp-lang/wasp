import {
  Bar,
  BarChart,
  CartesianGrid,
  Line,
  LineChart,
  XAxis,
  YAxis,
} from "recharts";
import {
  ChartContainer,
  ChartTooltip,
  ChartTooltipContent,
  type ChartConfig,
} from "../../components/ui/chart";
import type { GroupBy } from "../ranges";

const config = {
  clicks: { label: "Clicks", color: "var(--chart-1)" },
} satisfies ChartConfig;

/** Line chart for clicks per day, bar chart for clicks per referrer. */
export function StatsChart({
  groupBy,
  series,
}: {
  groupBy: GroupBy;
  series: { key: string; clicks: number }[];
}) {
  if (groupBy === "referrer" && series.length === 0) {
    return (
      <p className="text-muted-foreground py-10 text-center">
        No clicks in this time range.
      </p>
    );
  }
  return (
    <figure>
      <ChartContainer config={config} className="aspect-auto h-72 w-full">
        {groupBy === "day" ? (
          <LineChart data={series} margin={{ left: 4, right: 12, top: 8 }}>
            <CartesianGrid vertical={false} />
            <XAxis
              dataKey="key"
              tickLine={false}
              axisLine={false}
              tickMargin={8}
              minTickGap={24}
              tickFormatter={formatDay}
            />
            <YAxis
              allowDecimals={false}
              tickLine={false}
              axisLine={false}
              width={32}
            />
            <ChartTooltip
              content={
                <ChartTooltipContent
                  labelFormatter={(v) => formatDay(String(v))}
                />
              }
            />
            <Line
              dataKey="clicks"
              type="monotone"
              stroke="var(--color-clicks)"
              strokeWidth={2}
              dot={series.length <= 31}
            />
          </LineChart>
        ) : (
          <BarChart data={series} margin={{ left: 4, right: 12, top: 8 }}>
            <CartesianGrid vertical={false} />
            <XAxis
              dataKey="key"
              tickLine={false}
              axisLine={false}
              tickMargin={8}
            />
            <YAxis
              allowDecimals={false}
              tickLine={false}
              axisLine={false}
              width={32}
            />
            <ChartTooltip content={<ChartTooltipContent />} />
            <Bar dataKey="clicks" fill="var(--color-clicks)" radius={6} />
          </BarChart>
        )}
      </ChartContainer>
      {/* The same data as a table, for screen readers. */}
      <table className="sr-only" aria-label="Clicks data">
        <thead>
          <tr>
            <th>{groupBy === "day" ? "Day" : "Referrer"}</th>
            <th>Clicks</th>
          </tr>
        </thead>
        <tbody>
          {series.map((s) => (
            <tr key={s.key}>
              <td>{s.key}</td>
              <td>{s.clicks}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </figure>
  );
}

function formatDay(key: string): string {
  const d = new Date(`${key}T00:00:00Z`);
  return Number.isNaN(d.getTime())
    ? key
    : d.toLocaleDateString(undefined, {
        month: "short",
        day: "numeric",
        timeZone: "UTC",
      });
}
