import useBrokenLinks from "@docusaurus/useBrokenLinks";
import classNames from "classnames";
import SectionContainer from "./Layouts/SectionContainer";

const SHOWCASE_ID = "showcases";

const ShowcaseGallery = () => {
  useBrokenLinks().collectAnchor(SHOWCASE_ID);

  return (
    <SectionContainer className="space-y-16" id={SHOWCASE_ID}>
      <div className="grid grid-cols-12">
        <div className="col-span-12 text-center">
          <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
            🏆 Showcase Gallery 🏆
          </h2>
          <p className="text-neutral-500">
            See what others are building with Wasp.
          </p>
        </div>
      </div>

      <div
        className={`mx-auto grid max-w-lg gap-8 lg:max-w-none lg:grid-cols-3 lg:gap-12`}
      >
        <ShowcaseItem
          url="blog/2025/07/17/three-saas-case-studies-wasp"
          thumb="img/lp/showcase/kivo.webp"
          title="Kivo: Unified platform for creating data-driven reports"
          description="Learn how Kivo leveraged Wasp to build a complex, data-intensive application with a small team."
        >
          <Tag
            text="Data Analysis"
            className="border-yellow-600 bg-yellow-50 text-yellow-600"
          />
        </ShowcaseItem>

        <ShowcaseItem
          url="/blog/2022/11/26/erlis-amicus-usecase"
          thumb="img/lp/showcase/amicus.webp"
          title="Amicus: Task and workflow management for legal teams"
          description="See how Erlis rolled out fully-fledged SaaS as a team of one in record time and got first paying customers."
        >
          <Tag
            text="Legal"
            className="border-fuchsia-600 bg-fuchsia-50 text-fuchsia-600"
          />
        </ShowcaseItem>

        <ShowcaseItem
          url="https://searchcraft.io"
          thumb="img/lp/showcase/searchcraft.webp"
          title="Searchcraft: Advanced search developer tools"
          description="Searchcraft delivers purpose-built developer tools for content discovery."
        >
          <Tag
            text="Dev Tools"
            className="border-green-600 bg-green-50 text-green-600"
          />
        </ShowcaseItem>
      </div>
    </SectionContainer>
  );
};

const ShowcaseItem = ({ url, thumb, title, description, children }) => (
  <div>
    <a href={url} target="_blank">
      <div className="group inline-block min-w-full">
        <div className="flex flex-col space-y-3 pb-8 md:pb-0">
          <div
            className={`relative mb-4 h-56 w-full overflow-hidden rounded-lg border border-neutral-300 bg-black shadow-lg`}
          >
            <img src={thumb} className="object-cover" />
          </div>

          <h3 className="text-xl text-neutral-700">{title}</h3>

          <div className="flex space-x-2">{children}</div>

          <p className="text-base text-neutral-500">{description}</p>
        </div>
      </div>
    </a>
  </div>
);

const Tag = ({ text, className }) => (
  <span
    className={classNames(`rounded-md border px-2.5 py-0.5 text-sm`, className)}
  >
    {text}
  </span>
);

export default ShowcaseGallery;
