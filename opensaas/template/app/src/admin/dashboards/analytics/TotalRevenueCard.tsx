import { ArrowDown, ArrowUp, ShoppingCart } from "lucide-react";
import { useMemo } from "react";
import { type DailyStatsProps } from "../../../analytics/stats";
import {
  Card,
  CardContent,
  CardHeader,
} from "../../../client/components/ui/card";
import { cn } from "../../../client/utils";

export function TotalRevenueCard({
  dailyStats,
  weeklyStats,
  isLoading,
}: DailyStatsProps) {
  const isDeltaPositive = useMemo(() => {
    if (!weeklyStats) return false;
    return weeklyStats[0].totalRevenue - weeklyStats[1]?.totalRevenue > 0;
  }, [weeklyStats]);

  const deltaPercentage = useMemo(() => {
    if (!weeklyStats || weeklyStats.length < 2 || isLoading) return;
    if (
      weeklyStats[1]?.totalRevenue === 0 ||
      weeklyStats[0]?.totalRevenue === 0
    )
      return 0;

    weeklyStats.sort((a, b) => b.id - a.id);

    const percentage =
      ((weeklyStats[0].totalRevenue - weeklyStats[1]?.totalRevenue) /
        weeklyStats[1]?.totalRevenue) *
      100;
    return Math.floor(percentage);
  }, [isLoading, weeklyStats]);

  return (
    <Card>
      <CardHeader>
        <div className="bg-muted h-11.5 w-11.5 flex items-center justify-center rounded-full">
          <ShoppingCart className="size-6" />
        </div>
      </CardHeader>

      <CardContent className="flex justify-between">
        <div>
          <h4 className="text-title-md text-foreground font-bold">
            ${dailyStats?.totalRevenue}
          </h4>
          <span className="text-muted-foreground text-sm font-medium">
            Total Revenue
          </span>
        </div>

        <span
          className={cn("flex items-center gap-1 text-sm font-medium", {
            "text-success":
              isDeltaPositive && !isLoading && deltaPercentage !== 0,
            "text-destructive":
              !isDeltaPositive && !isLoading && deltaPercentage !== 0,
            "text-muted-foreground":
              isLoading || !deltaPercentage || deltaPercentage === 0,
          })}
        >
          {isLoading
            ? "..."
            : deltaPercentage && deltaPercentage !== 0
              ? `${deltaPercentage}%`
              : "-"}
          {!isLoading &&
            deltaPercentage &&
            deltaPercentage !== 0 &&
            (isDeltaPositive ? <ArrowUp /> : <ArrowDown />)}
        </span>
      </CardContent>
    </Card>
  );
}
