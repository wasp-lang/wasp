import SectionContainer from "./Layouts/SectionContainer";

const roadmapUrl = "https://github.com/orgs/wasp-lang/projects/5";

const Roadmap = () => (
  <SectionContainer className="lg:py-18 space-y-16" id="roadmap">
    <div className="grid grid-cols-12">
      <div className="col-span-12 text-center">
        <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
          🚧 Roadmap 🚧
        </h2>
        <p className="text-neutral-500">
          Work on Wasp never stops:&nbsp;
          <a
            href={roadmapUrl}
            className="font-medium underline decoration-yellow-500 decoration-2"
          >
            get a glimpse
          </a>
          &nbsp;of what is coming next!
        </p>
      </div>
    </div>

    <div className="grid grid-cols-12">
      <div className="col-span-12 flex justify-center lg:col-span-8 lg:col-start-3">
        <a href={roadmapUrl}>
          <img className="" src="img/lp/wasp-roadmap.webp" alt="Roadmap" />
        </a>
      </div>
    </div>
  </SectionContainer>
);

export default Roadmap;
