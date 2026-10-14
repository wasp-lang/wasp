import { ComponentProps, useId } from "react";
import { twJoin } from "tailwind-merge";

interface ColorRadioButtonProps extends Required<
  Pick<
    ComponentProps<"input">,
    "name" | "checked" | "value" | "onChange" | "title"
  >
> {
  bgColor: string;
}

export function ColorRadioButton({
  checked,
  bgColor,
  title,
  ...props
}: ColorRadioButtonProps) {
  const id = useId();

  return (
    <div className="relative flex items-center">
      <label
        htmlFor={id}
        className={twJoin(
          "flex h-8 w-8 cursor-pointer items-center justify-center rounded-full border border-neutral-900 transition-transform",
          "hover:scale-110",
          checked && "ring-2 ring-neutral-900 ring-offset-2",
        )}
        style={{ background: bgColor }}
        title={title}
      >
        <input
          type="radio"
          className="hidden"
          id={id}
          checked={checked}
          {...props}
        />
      </label>
    </div>
  );
}
