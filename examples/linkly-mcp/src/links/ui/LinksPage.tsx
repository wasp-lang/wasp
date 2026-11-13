import { MoreHorizontal, Plus } from "lucide-react";
import { useEffect, useState } from "react";
import { Link as RouterLink, useNavigate, useSearchParams } from "react-router";
import { getLinks, useQuery } from "wasp/client/operations";
import { CopyButton } from "../../app/ui/CopyButton";
import { errorMessage } from "../../app/ui/errors";
import { Badge } from "../../components/ui/badge";
import { Button } from "../../components/ui/button";
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from "../../components/ui/dropdown-menu";
import { Input } from "../../components/ui/input";
import { Label } from "../../components/ui/label";
import { Skeleton } from "../../components/ui/skeleton";
import { Switch } from "../../components/ui/switch";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "../../components/ui/table";
import { dashboardPaths } from "../../deeplinks/urls";
import type { LinkOut } from "../schemas";
import { DeleteLinkDialog } from "./DeleteLinkDialog";

// Honors every parameter from dashboardUrls.links(): ?search= and ?tag=.
export function LinksPage() {
  const [params, setParams] = useSearchParams();
  const navigate = useNavigate();
  const search = params.get("search") ?? "";
  const tag = params.get("tag") ?? "";
  const archived = params.get("archived") === "true";

  const [searchInput, setSearchInput] = useState(search);
  const [cursors, setCursors] = useState<(string | undefined)[]>([undefined]);
  const cursor = cursors[cursors.length - 1];
  const [toDelete, setToDelete] = useState<LinkOut | null>(null);

  useEffect(() => setSearchInput(search), [search]);
  useEffect(() => setCursors([undefined]), [search, tag, archived]);

  const { data, isLoading, error } = useQuery(getLinks, {
    search: search || undefined,
    tag: tag || undefined,
    archived,
    cursor,
    limit: 25,
  });

  function setParam(key: string, value: string | null) {
    const next = new URLSearchParams(params);
    if (value) next.set(key, value);
    else next.delete(key);
    setParams(next, { replace: true });
  }

  return (
    <div className="space-y-6">
      <div className="flex flex-wrap items-center justify-between gap-3">
        <h1 className="font-heading text-2xl font-semibold">Links</h1>
        <Button asChild>
          <RouterLink to={dashboardPaths.newLink()}>
            <Plus data-icon="inline-start" /> New link
          </RouterLink>
        </Button>
      </div>

      <div className="flex flex-wrap items-center gap-3">
        <form
          className="flex-1"
          onSubmit={(e) => {
            e.preventDefault();
            setParam("search", searchInput.trim() || null);
          }}
        >
          <Input
            type="search"
            aria-label="Search links"
            placeholder="Search slug, title, or destination…"
            value={searchInput}
            onChange={(e) => setSearchInput(e.target.value)}
            onBlur={() => setParam("search", searchInput.trim() || null)}
          />
        </form>
        {tag && (
          <Badge variant="secondary" className="gap-1">
            tag: {tag}
            <button
              type="button"
              aria-label="Clear tag filter"
              className="ml-1"
              onClick={() => setParam("tag", null)}
            >
              ×
            </button>
          </Badge>
        )}
        <div className="flex items-center gap-2">
          <Switch
            id="show-archived"
            checked={archived}
            onCheckedChange={(v) => setParam("archived", v ? "true" : null)}
          />
          <Label htmlFor="show-archived">Archived</Label>
        </div>
      </div>

      {error && <p className="text-destructive">{errorMessage(error)}</p>}

      <div className="overflow-x-auto rounded-lg border">
        <Table>
          <TableHeader>
            <TableRow>
              <TableHead>Short link</TableHead>
              <TableHead>Destination</TableHead>
              <TableHead>Tags</TableHead>
              <TableHead>Created</TableHead>
              <TableHead className="w-10">
                <span className="sr-only">Actions</span>
              </TableHead>
            </TableRow>
          </TableHeader>
          <TableBody>
            {isLoading &&
              Array.from({ length: 3 }).map((_, i) => (
                <TableRow key={i}>
                  <TableCell colSpan={5}>
                    <Skeleton className="h-5 w-full" />
                  </TableCell>
                </TableRow>
              ))}
            {data?.data.length === 0 && (
              <TableRow>
                <TableCell
                  colSpan={5}
                  className="text-muted-foreground py-10 text-center"
                >
                  {search || tag
                    ? "No links match."
                    : archived
                      ? "No archived links."
                      : "No links yet. Create your first one."}
                </TableCell>
              </TableRow>
            )}
            {data?.data.map((link) => (
              <TableRow key={link.id}>
                <TableCell>
                  <div className="flex items-center gap-1">
                    <RouterLink
                      className="font-medium hover:underline"
                      to={dashboardPaths.link(link.id)}
                    >
                      /r/{link.slug}
                    </RouterLink>
                    <CopyButton
                      value={link.shortUrl}
                      label={`Copy ${link.shortUrl}`}
                    />
                  </div>
                  {link.title && (
                    <div className="text-muted-foreground text-sm">
                      {link.title}
                    </div>
                  )}
                </TableCell>
                <TableCell
                  className="max-w-xs truncate"
                  title={link.destinationUrl}
                >
                  {link.destinationUrl}
                </TableCell>
                <TableCell>
                  <div className="flex flex-wrap gap-1">
                    {link.tags.map((t) => (
                      <Badge key={t} variant="outline" asChild>
                        <button
                          type="button"
                          onClick={() => setParam("tag", t)}
                        >
                          {t}
                        </button>
                      </Badge>
                    ))}
                  </div>
                </TableCell>
                <TableCell className="text-muted-foreground whitespace-nowrap">
                  {new Date(link.createdAt).toLocaleDateString()}
                </TableCell>
                <TableCell>
                  <DropdownMenu>
                    <DropdownMenuTrigger asChild>
                      <Button
                        variant="ghost"
                        size="icon-sm"
                        aria-label={`Actions for /r/${link.slug}`}
                      >
                        <MoreHorizontal />
                      </Button>
                    </DropdownMenuTrigger>
                    <DropdownMenuContent align="end">
                      <DropdownMenuItem
                        onSelect={() =>
                          navigate(
                            dashboardPaths.link(link.id, { tab: "edit" }),
                          )
                        }
                      >
                        Edit
                      </DropdownMenuItem>
                      <DropdownMenuItem
                        onSelect={() =>
                          navigate(
                            dashboardPaths.link(link.id, { tab: "stats" }),
                          )
                        }
                      >
                        Stats
                      </DropdownMenuItem>
                      <DropdownMenuItem
                        onSelect={() =>
                          navigator.clipboard.writeText(link.shortUrl)
                        }
                      >
                        Copy short URL
                      </DropdownMenuItem>
                      <DropdownMenuSeparator />
                      <DropdownMenuItem
                        variant="destructive"
                        onSelect={() => setToDelete(link)}
                      >
                        Delete
                      </DropdownMenuItem>
                    </DropdownMenuContent>
                  </DropdownMenu>
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </div>

      <div className="flex justify-end gap-2">
        <Button
          variant="outline"
          size="sm"
          disabled={cursors.length === 1}
          onClick={() => setCursors((c) => c.slice(0, -1))}
        >
          Previous
        </Button>
        <Button
          variant="outline"
          size="sm"
          disabled={!data?.nextCursor}
          onClick={() =>
            data?.nextCursor && setCursors((c) => [...c, data.nextCursor!])
          }
        >
          Next
        </Button>
      </div>

      {toDelete && (
        <DeleteLinkDialog
          link={toDelete}
          open={!!toDelete}
          onOpenChange={(open) => !open && setToDelete(null)}
          onDeleted={() => setToDelete(null)}
        />
      )}
    </div>
  );
}
