import Link from "@docusaurus/Link";
import { ComponentProps } from "react";
import { twMerge } from "tailwind-merge";

const variantClassNames = {
  yellow: "decoration-wasp-yellow hover:bg-wasp-yellow-light",
  "yellow-dark": "decoration-wasp-yellow-dark hover:bg-wasp-yellow",
  purple: "decoration-wasp-purple hover:bg-wasp-purple-light",
};

const TextLink = ({
  variant = "yellow",
  className,
  ...props
}: ComponentProps<typeof Link> & {
  to: string;
  variant?: keyof typeof variantClassNames;
}) => (
  <Link
    {...props}
    className={twMerge(
      "box-decoration-clone px-0.5 text-wasp-black",
      "underline decoration-2 underline-offset-2",
      "transition-colors duration-200 ease-out",
      "hover:text-wasp-black hover:no-underline",
      variantClassNames[variant],
      className,
    )}
  />
);

export default TextLink;
