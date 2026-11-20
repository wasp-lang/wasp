import { ClassNameValue, twJoin } from "tailwind-merge";
import { Tag } from "wasp/entities";

type TagLabelSize = "md" | "sm" | "tiny";

interface TagLabelProps {
  tag: Pick<Tag, "id" | "color" | "name">;
  isActive: boolean;
  size?: TagLabelSize;
  showColorCircle?: boolean;
}

export function TagLabel({
  tag,
  isActive,
  size = "md",
  showColorCircle = false,
}: TagLabelProps) {
  return (
    <span
      className={twJoin(
        "border-line inline-flex max-w-full items-center gap-2 rounded-md border font-medium",
        sizeStyles[size],
      )}
      style={{
        backgroundColor: isActive ? tag.color : "transparent",
        color: isActive ? "var(--on-accent)" : undefined,
      }}
    >
      {tag.name}
      {showColorCircle && (
        <span
          className={twJoin(
            "bg-surface shrink-0 rounded-full border border-black/20",
            colorCircleSizeStyles[size],
          )}
          style={{
            backgroundColor: isActive ? undefined : tag.color,
          }}
        />
      )}
    </span>
  );
}

const sizeStyles: Record<TagLabelSize, ClassNameValue> = {
  md: "px-4 py-1.5 text-sm",
  sm: "px-3 py-1 text-xs",
  tiny: "px-2 py-0.5 text-xs",
};

const colorCircleSizeStyles: Record<TagLabelSize, ClassNameValue> = {
  md: "size-3",
  sm: "size-2",
  tiny: "size-1.5",
};
