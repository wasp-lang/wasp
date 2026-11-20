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
          "flex size-8 cursor-pointer items-center justify-center border-2 border-black",
          "hover:outline-2 hover:outline-offset-2 hover:outline-black",
          checked && "outline-2 outline-offset-2 outline-black",
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
