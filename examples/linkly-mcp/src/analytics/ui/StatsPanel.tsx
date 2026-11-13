import { useSearchParams } from "react-router";
import { getLinkStats, useQuery } from "wasp/client/operations";
import { errorMessage } from "../../app/ui/errors";
import { Skeleton } from "../../components/ui/skeleton";
import { Tabs, TabsList, TabsTrigger } from "../../components/ui/tabs";
import type { GroupBy } from "../ranges";
import { parseRange, RangeSelect } from "./RangeSelect";
import { StatsChart } from "./StatsChart";

// Range and groupBy come from the URL (?range=&groupBy=) so deep links such
// as dashboardUrls.link(id, { tab: "stats", range: "30d" }) preselect them.
export function StatsPanel({ linkId }: { linkId: string }) {
  const [params, setParams] = useSearchParams();
  const range = parseRange(params.get("range"));
  const groupBy: GroupBy =
    params.get("groupBy") === "referrer" ? "referrer" : "day";

  const { data, isLoading, error } = useQuery(getLinkStats, {
    linkId,
    range,
    groupBy,
  });

  function setParam(key: string, value: string) {
    const next = new URLSearchParams(params);
    next.set(key, value);
    setParams(next, { replace: true });
  }

  return (
    <div className="space-y-4">
      <div className="flex flex-wrap items-center gap-3">
        <RangeSelect value={range} onChange={(r) => setParam("range", r)} />
        <Tabs value={groupBy} onValueChange={(v) => setParam("groupBy", v)}>
          <TabsList>
            <TabsTrigger value="day">By day</TabsTrigger>
            <TabsTrigger value="referrer">By referrer</TabsTrigger>
          </TabsList>
        </Tabs>
        {data && (
          <p className="text-muted-foreground ml-auto text-sm">
            <span
              className="font-heading text-foreground text-2xl font-semibold"
              data-testid="total-clicks"
            >
              {data.total}
            </span>{" "}
            {data.total === 1 ? "click" : "clicks"}
          </p>
        )}
      </div>
      {isLoading && <Skeleton className="h-72 w-full" />}
      {error && <p className="text-destructive">{errorMessage(error)}</p>}
      {data && <StatsChart groupBy={data.groupBy} series={data.series} />}
    </div>
  );
}
