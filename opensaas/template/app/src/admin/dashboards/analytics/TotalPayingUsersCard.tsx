import { ArrowDown, ArrowUp, ShoppingBag } from "lucide-react";
import { useMemo } from "react";
import { type DailyStatsProps } from "../../../analytics/stats";
import {
  Card,
  CardContent,
  CardHeader,
} from "../../../client/components/ui/card";
import { cn } from "../../../client/utils";

export function TotalPayingUsersCard({
  dailyStats,
  isLoading,
}: DailyStatsProps) {
  const isDeltaPositive = useMemo(() => {
    return !!dailyStats?.paidUserDelta && dailyStats?.paidUserDelta > 0;
  }, [dailyStats]);

  return (
    <Card>
      <CardHeader>
        <div className="bg-muted h-11.5 w-11.5 flex items-center justify-center rounded-full">
          <ShoppingBag className="size-6" />
        </div>
      </CardHeader>

      <CardContent className="flex justify-between">
        <div>
          <h4 className="text-title-md text-foreground font-bold">
            {dailyStats?.paidUserCount}
          </h4>
          <span className="text-muted-foreground text-sm font-medium">
            Total Paying Users
          </span>
        </div>

        <span
          className={cn("flex items-center gap-1 text-sm font-medium", {
            "text-success": isDeltaPositive && !isLoading,
            "text-destructive":
              !isDeltaPositive && !isLoading && dailyStats?.paidUserDelta !== 0,
            "text-muted-foreground": isLoading || !dailyStats?.paidUserDelta,
          })}
        >
          {isLoading ? "..." : (dailyStats?.paidUserDelta ?? "-")}
          {!isLoading &&
            (dailyStats?.paidUserDelta ?? 0) !== 0 &&
            (isDeltaPositive ? <ArrowUp /> : <ArrowDown />)}
        </span>
      </CardContent>
    </Card>
  );
}
