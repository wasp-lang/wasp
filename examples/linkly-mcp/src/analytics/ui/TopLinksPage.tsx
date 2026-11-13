import { Link as RouterLink, useSearchParams } from "react-router";
import { getTopLinks, useQuery } from "wasp/client/operations";
import { errorMessage } from "../../app/ui/errors";
import { Skeleton } from "../../components/ui/skeleton";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "../../components/ui/table";
import { dashboardPaths } from "../../deeplinks/urls";
import { parseRange, RangeSelect } from "./RangeSelect";

// Honors dashboardUrls.topLinks({ range }).
export function TopLinksPage() {
  const [params, setParams] = useSearchParams();
  const range = parseRange(params.get("range"));
  const { data, isLoading, error } = useQuery(getTopLinks, {
    range,
    limit: 20,
  });

  return (
    <div className="space-y-6">
      <div className="flex flex-wrap items-center justify-between gap-3">
        <h1 className="font-heading text-2xl font-semibold">Top links</h1>
        <RangeSelect
          value={range}
          onChange={(r) => setParams({ range: r }, { replace: true })}
        />
      </div>
      {error && <p className="text-destructive">{errorMessage(error)}</p>}
      <div className="overflow-x-auto rounded-lg border">
        <Table>
          <TableHeader>
            <TableRow>
              <TableHead>Link</TableHead>
              <TableHead>Destination</TableHead>
              <TableHead className="text-right">Clicks</TableHead>
            </TableRow>
          </TableHeader>
          <TableBody>
            {isLoading && (
              <TableRow>
                <TableCell colSpan={3}>
                  <Skeleton className="h-5 w-full" />
                </TableCell>
              </TableRow>
            )}
            {data?.data.length === 0 && (
              <TableRow>
                <TableCell
                  colSpan={3}
                  className="text-muted-foreground py-10 text-center"
                >
                  No clicks in this time range.
                </TableCell>
              </TableRow>
            )}
            {data?.data.map(({ link, clicks }) => (
              <TableRow key={link.id}>
                <TableCell>
                  <RouterLink
                    className="font-medium hover:underline"
                    to={dashboardPaths.link(link.id, { tab: "stats", range })}
                  >
                    /r/{link.slug}
                  </RouterLink>
                </TableCell>
                <TableCell className="text-muted-foreground max-w-sm truncate">
                  {link.destinationUrl}
                </TableCell>
                <TableCell className="text-right font-medium tabular-nums">
                  {clicks}
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </div>
    </div>
  );
}
