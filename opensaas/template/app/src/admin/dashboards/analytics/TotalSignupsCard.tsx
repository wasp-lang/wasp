import { ArrowUp, UsersRound } from "lucide-react";
import { useMemo } from "react";
import { type DailyStatsProps } from "../../../analytics/stats";
import {
  Card,
  CardContent,
  CardHeader,
} from "../../../client/components/ui/card";
import { cn } from "../../../client/utils";

export function TotalSignupsCard({ dailyStats, isLoading }: DailyStatsProps) {
  const isDeltaPositive = useMemo(() => {
    return !!dailyStats?.userDelta && dailyStats.userDelta > 0;
  }, [dailyStats]);

  return (
    <Card>
      <CardHeader>
        <div className="bg-muted h-11.5 w-11.5 flex items-center justify-center rounded-full">
          <UsersRound className="size-6" />
        </div>
      </CardHeader>

      <CardContent className="flex justify-between">
        <div>
          <h4 className="text-title-md text-foreground font-bold">
            {dailyStats?.userCount}
          </h4>
          <span className="text-muted-foreground text-sm font-medium">
            Total Signups
          </span>
        </div>

        <span
          className={cn("flex items-center gap-1 text-sm font-medium", {
            "text-success": isDeltaPositive && !isLoading,
            "text-destructive":
              !isDeltaPositive && !isLoading && dailyStats?.userDelta !== 0,
            "text-muted-foreground": isLoading || !dailyStats?.userDelta,
          })}
        >
          {isLoading ? "..." : (dailyStats?.userDelta ?? "-")}
          {!isLoading && (dailyStats?.userDelta ?? 0) > 0 && <ArrowUp />}
        </span>
      </CardContent>
    </Card>
  );
}
