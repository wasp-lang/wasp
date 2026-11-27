import { useNavigate, useSearchParams } from "react-router";
import { toast } from "sonner";
import { createLink } from "wasp/client/operations";
import { errorMessage } from "../../app/ui/errors";
import { dashboardPaths } from "../../deeplinks/urls";
import { LinkForm, splitTags } from "./LinkForm";

// Honors dashboardUrls.newLink(prefill): query params prefill the form, so an
// agent can hand the user a ready-to-submit form.
export function NewLinkPage() {
  const [params] = useSearchParams();
  const navigate = useNavigate();

  return (
    <div className="space-y-6">
      <h1 className="font-heading text-2xl font-semibold">New link</h1>
      <LinkForm
        submitLabel="Create link"
        defaultValues={{
          destinationUrl: params.get("destinationUrl") ?? "",
          slug: params.get("slug") ?? "",
          title: params.get("title") ?? "",
          tags: params.get("tags") ?? "",
        }}
        onSubmit={async (values) => {
          try {
            const link = await createLink({
              destinationUrl: values.destinationUrl,
              slug: values.slug || undefined,
              title: values.title || undefined,
              tags: splitTags(values.tags),
            });
            toast.success(`Created /r/${link.slug}`);
            navigate(dashboardPaths.link(link.id));
          } catch (e) {
            toast.error(errorMessage(e));
          }
        }}
      />
    </div>
  );
}
