import { CopyButton } from "./CopyButton";

/** A labeled value shown as visible text (so agents and people can read it), with a copy button. */
export function CopyField({
  label,
  value,
  testId,
}: {
  label: string;
  value: string;
  testId?: string;
}) {
  return (
    <div className="space-y-1">
      <div className="text-sm font-medium">{label}</div>
      <div className="bg-muted/40 flex items-center gap-2 rounded-md border py-1 pr-1 pl-3">
        <code className="min-w-0 flex-1 text-sm break-all" data-testid={testId}>
          {value}
        </code>
        <CopyButton value={value} label={`Copy ${label}`} />
      </div>
    </div>
  );
}
