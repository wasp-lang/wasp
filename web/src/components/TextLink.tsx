import Link from "@docusaurus/Link";
import { ReactNode } from "react";
import { twMerge } from "tailwind-merge";

const variantClassNames = {
  yellow: "decoration-wasp-yellow hover:bg-wasp-yellow-light",
  "yellow-dark": "decoration-wasp-yellow-dark hover:bg-wasp-yellow",
  purple: "decoration-wasp-purple hover:bg-wasp-purple-light",
};

const TextLink = ({
  to,
  variant = "yellow",
  className,
  children,
}: {
  to: string;
  variant?: keyof typeof variantClassNames;
  className?: string;
  children: ReactNode;
}) => (
  <Link
    to={to}
    className={twMerge(
      "box-decoration-clone px-0.5 text-wasp-black",
      "underline decoration-2 underline-offset-2",
      "transition-colors duration-200 ease-out",
      "hover:text-wasp-black hover:no-underline",
      variantClassNames[variant],
      className,
    )}
  >
    {children}
  </Link>
);

export default TextLink;
