import { ExternalLink, Trash2 } from "lucide-react";
import { useState } from "react";
import { useNavigate, useParams, useSearchParams } from "react-router";
import { toast } from "sonner";
import { getLink, updateLink, useQuery } from "wasp/client/operations";
import { StatsPanel } from "../../analytics/ui/StatsPanel";
import { CopyButton } from "../../app/ui/CopyButton";
import { errorMessage } from "../../app/ui/errors";
import { Badge } from "../../components/ui/badge";
import { Button } from "../../components/ui/button";
import { Skeleton } from "../../components/ui/skeleton";
import {
  Tabs,
  TabsContent,
  TabsList,
  TabsTrigger,
} from "../../components/ui/tabs";
import { dashboardPaths } from "../../deeplinks/urls";
import { DeleteLinkDialog } from "./DeleteLinkDialog";
import { LinkForm, splitTags } from "./LinkForm";

// Honors dashboardUrls.link(id, { tab, range, groupBy }). The tab lives in
// the URL so deep links can open the stats view directly.
export function LinkDetailPage() {
  const { id = "" } = useParams();
  const [params, setParams] = useSearchParams();
  const navigate = useNavigate();
  const tab = params.get("tab") === "stats" ? "stats" : "edit";
  const [deleting, setDeleting] = useState(false);

  const { data: link, isLoading, error } = useQuery(getLink, { id });

  if (isLoading) return <Skeleton className="h-40 w-full" />;
  if (error || !link)
    return (
      <p className="text-destructive">
        {errorMessage(error ?? "Link not found.")}
      </p>
    );

  return (
    <div className="space-y-6">
      <div className="flex flex-wrap items-start justify-between gap-3">
        <div className="space-y-1">
          <h1 className="font-heading flex items-center gap-2 text-2xl font-semibold">
            /r/{link.slug}
            {link.archived && <Badge variant="secondary">Archived</Badge>}
          </h1>
          <div className="text-muted-foreground flex items-center gap-1 text-sm">
            <a
              href={link.shortUrl}
              target="_blank"
              rel="noreferrer"
              className="hover:underline"
            >
              {link.shortUrl}
            </a>
            <CopyButton value={link.shortUrl} label="Copy short URL" />
          </div>
          <div className="text-muted-foreground flex items-center gap-1 text-sm">
            <ExternalLink className="size-3.5" />
            <span className="max-w-md truncate">{link.destinationUrl}</span>
          </div>
        </div>
        <Button variant="destructive" onClick={() => setDeleting(true)}>
          <Trash2 data-icon="inline-start" /> Delete
        </Button>
      </div>

      <Tabs
        value={tab}
        onValueChange={(value) => {
          const next = new URLSearchParams(params);
          next.set("tab", value);
          setParams(next, { replace: true });
        }}
      >
        <TabsList>
          <TabsTrigger value="edit">Edit</TabsTrigger>
          <TabsTrigger value="stats">Stats</TabsTrigger>
        </TabsList>
        <TabsContent value="edit" className="pt-4">
          <LinkForm
            key={link.updatedAt}
            submitLabel="Save changes"
            showArchived
            defaultValues={{
              destinationUrl: link.destinationUrl,
              slug: link.slug,
              title: link.title ?? "",
              tags: link.tags.join(", "),
              archived: link.archived,
            }}
            onSubmit={async (values) => {
              try {
                await updateLink({
                  id: link.id,
                  destinationUrl: values.destinationUrl,
                  slug: values.slug || undefined,
                  title: values.title || null,
                  tags: splitTags(values.tags),
                  archived: values.archived,
                });
                toast.success("Saved");
              } catch (e) {
                toast.error(errorMessage(e));
              }
            }}
          />
        </TabsContent>
        <TabsContent value="stats" className="pt-4">
          <StatsPanel linkId={link.id} />
        </TabsContent>
      </Tabs>

      <DeleteLinkDialog
        link={link}
        open={deleting}
        onOpenChange={setDeleting}
        onDeleted={() => navigate(dashboardPaths.links())}
      />
    </div>
  );
}
