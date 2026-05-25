import classNames from "classnames";

const SectionContainer = ({ children, className, id }) => (
  <div
    className={classNames(
      "sm:py-18 mx-auto px-6 py-16 lg:container",
      "md:py-24",
      "lg:px-16 lg:py-24 xl:px-20",
      className,
    )}
    id={id}
  >
    {children}
  </div>
);

export default SectionContainer;
