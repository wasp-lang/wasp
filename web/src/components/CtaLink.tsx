import { ComponentProps } from "react";
import { twMerge } from "tailwind-merge";

import TextLink from "./TextLink";

const CtaLink = ({ className, ...props }: ComponentProps<typeof TextLink>) => (
  <TextLink
    {...props}
    className={twMerge(
      "inline-flex items-center gap-2 font-mono text-base font-bold",
      "underline-offset-4",
      className,
    )}
  />
);

export default CtaLink;
