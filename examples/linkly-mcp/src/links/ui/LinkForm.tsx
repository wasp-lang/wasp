import { zodResolver } from "@hookform/resolvers/zod";
import { Controller, useForm } from "react-hook-form";
import { z } from "zod";
import { Button } from "../../components/ui/button";
import {
  Field,
  FieldDescription,
  FieldError,
  FieldGroup,
  FieldLabel,
} from "../../components/ui/field";
import { Input } from "../../components/ui/input";
import { Switch } from "../../components/ui/switch";
import { destinationUrlSchema, SLUG_PATTERN } from "../schemas";

// The form edits strings; tags are typed comma-separated. It reuses the
// slice's field schemas so the dashboard validates what REST and MCP validate.
const formSchema = z.object({
  destinationUrl: destinationUrlSchema,
  slug: z
    .string()
    .trim()
    .toLowerCase()
    .refine(
      (v) => v === "" || SLUG_PATTERN.test(v),
      "3–40 characters: lowercase letters, numbers, and hyphens.",
    ),
  title: z.string().trim().max(200),
  tags: z
    .string()
    .refine((v) => splitTags(v).length <= 10, "Up to 10 tags.")
    .refine(
      (v) => splitTags(v).every((t) => t.length <= 30),
      "Each tag can be up to 30 characters.",
    ),
  archived: z.boolean(),
});

export type LinkFormValues = z.infer<typeof formSchema>;

export function splitTags(value: string): string[] {
  return [
    ...new Set(
      value
        .split(",")
        .map((t) => t.trim().toLowerCase())
        .filter(Boolean),
    ),
  ];
}

export function LinkForm({
  defaultValues,
  submitLabel,
  showArchived = false,
  onSubmit,
}: {
  defaultValues: Partial<LinkFormValues>;
  submitLabel: string;
  showArchived?: boolean;
  onSubmit: (values: LinkFormValues) => Promise<void>;
}) {
  const form = useForm<LinkFormValues>({
    resolver: zodResolver(formSchema),
    defaultValues: {
      destinationUrl: "",
      slug: "",
      title: "",
      tags: "",
      archived: false,
      ...defaultValues,
    },
  });

  return (
    <form
      onSubmit={form.handleSubmit(onSubmit)}
      className="max-w-xl"
      noValidate
    >
      <FieldGroup>
        <Controller
          name="destinationUrl"
          control={form.control}
          render={({ field, fieldState }) => (
            <Field data-invalid={fieldState.invalid}>
              <FieldLabel htmlFor="destinationUrl">Destination URL</FieldLabel>
              <Input
                {...field}
                id="destinationUrl"
                placeholder="https://example.com/landing-page"
                aria-invalid={fieldState.invalid}
              />
              {fieldState.error && <FieldError errors={[fieldState.error]} />}
            </Field>
          )}
        />
        <Controller
          name="slug"
          control={form.control}
          render={({ field, fieldState }) => (
            <Field data-invalid={fieldState.invalid}>
              <FieldLabel htmlFor="slug">Slug</FieldLabel>
              <Input
                {...field}
                id="slug"
                placeholder="summer-sale"
                aria-invalid={fieldState.invalid}
              />
              <FieldDescription>
                The short URL is /r/&lt;slug&gt;. Leave empty for a random one.
              </FieldDescription>
              {fieldState.error && <FieldError errors={[fieldState.error]} />}
            </Field>
          )}
        />
        <Controller
          name="title"
          control={form.control}
          render={({ field, fieldState }) => (
            <Field data-invalid={fieldState.invalid}>
              <FieldLabel htmlFor="title">Title</FieldLabel>
              <Input
                {...field}
                id="title"
                placeholder="Summer sale landing page"
                aria-invalid={fieldState.invalid}
              />
              {fieldState.error && <FieldError errors={[fieldState.error]} />}
            </Field>
          )}
        />
        <Controller
          name="tags"
          control={form.control}
          render={({ field, fieldState }) => (
            <Field data-invalid={fieldState.invalid}>
              <FieldLabel htmlFor="tags">Tags</FieldLabel>
              <Input
                {...field}
                id="tags"
                placeholder="promo, q3"
                aria-invalid={fieldState.invalid}
              />
              <FieldDescription>Comma-separated, up to 10.</FieldDescription>
              {fieldState.error && <FieldError errors={[fieldState.error]} />}
            </Field>
          )}
        />
        {showArchived && (
          <Controller
            name="archived"
            control={form.control}
            render={({ field }) => (
              <Field orientation="horizontal">
                <Switch
                  id="archived"
                  checked={field.value}
                  onCheckedChange={field.onChange}
                />
                <FieldLabel htmlFor="archived">
                  Archived (the short URL stops redirecting)
                </FieldLabel>
              </Field>
            )}
          />
        )}
        <div>
          <Button type="submit" disabled={form.formState.isSubmitting}>
            {form.formState.isSubmitting ? "Saving…" : submitLabel}
          </Button>
        </div>
      </FieldGroup>
    </form>
  );
}
