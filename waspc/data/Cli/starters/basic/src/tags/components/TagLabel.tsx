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
        "inline-flex items-center gap-1.5 border border-neutral-900 font-mono font-semibold text-neutral-900",
        sizeStyles[size],
      )}
      style={{
        backgroundColor: isActive ? tag.color : "transparent",
      }}
    >
      {tag.name}
      {showColorCircle && (
        <span
          className={twJoin(
            "rounded-full border border-neutral-900 bg-white",
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
  md: "px-3 py-1.5 text-xs",
  sm: "px-2.5 py-1 text-xs",
  tiny: "px-2 py-0.5 text-xs",
};

const colorCircleSizeStyles: Record<TagLabelSize, ClassNameValue> = {
  md: "h-3 w-3",
  sm: "h-2 w-2",
  tiny: "h-1.5 w-1.5",
};
