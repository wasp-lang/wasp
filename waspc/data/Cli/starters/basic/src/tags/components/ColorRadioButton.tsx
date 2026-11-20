import { ComponentProps, useId } from "react";
import { twJoin } from "tailwind-merge";

interface ColorRadioButtonProps
  extends Required<
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
      <input
        type="radio"
        className="peer sr-only"
        id={id}
        checked={checked}
        aria-label={title}
        {...props}
      />
      <label
        htmlFor={id}
        className={twJoin(
          "border-line flex size-8 cursor-pointer items-center justify-center rounded-md border",
          "peer-focus-visible:outline-ink peer-focus-visible:outline-2 peer-focus-visible:outline-offset-2",
          checked && "outline-ink outline-2 outline-offset-2",
        )}
        style={{ background: bgColor }}
        title={title}
      />
    </div>
  );
}
