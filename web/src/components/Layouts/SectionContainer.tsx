import classNames from "classnames";
import { ReactNode } from "react";

interface SectionContainerProps {
  children: ReactNode;
  className?: string;
  id?: string;
}

const SectionContainer = ({
  children,
  className,
  id,
}: SectionContainerProps) => (
  <div
    className={classNames(
      "mx-auto lg:container",
      "px-6 md:px-12 lg:px-16",
      "py-10 md:py-12 lg:py-16",
      className,
    )}
    id={id}
  >
    {children}
  </div>
);

export default SectionContainer;
