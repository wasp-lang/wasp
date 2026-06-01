import classNames from "classnames";

const SectionContainer = ({
  children,
  className = undefined,
  id = undefined,
}) => (
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
