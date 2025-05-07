import React from 'react'
import classNames from 'classnames'
import SectionContainer from './Layouts/SectionContainer'

const Tag = ({ text, className }) => (
  <span
    className={classNames(
      `
      rounded-md border px-2.5 py-0.5 text-sm

    `,
      className
    )}
  >
    {text}
  </span>
)

const ShowcaseItem = ({ url, thumb, title, description, children }) => (
  <div>
    <a href={url}>
      <div className="group inline-block min-w-full">
        <div className="flex flex-col space-y-3 pb-8 md:pb-0">
          <div
            className={`
              relative mb-4 h-40 w-full overflow-auto overflow-y-hidden
              rounded-lg border border-neutral-300 shadow-lg
            `}
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
)

const ShowcaseGallery = () => {
  return (
    <SectionContainer className="space-y-16" id="showcases">
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
        className={`
          mx-auto grid max-w-lg gap-8
          lg:max-w-none lg:grid-cols-3 lg:gap-12
        `}
      >
        <ShowcaseItem
          url="https://searchcraft.io"
          thumb="img/lp/showcase/searchcraft.webp"
          title="Searchcraft"
          description="Fast, easy search tools for frontend developers."
        >
          <Tag
            text="developer-tools"
            className="border-yellow-600 bg-yellow-50 text-yellow-600"
          />
          <Tag
            text="startup"
            className="border-slate-500 bg-slate-50 text-slate-500"
          />
        </ShowcaseItem>

        <ShowcaseItem
          url="https://www.promptpanda.io/"
          thumb="img/lp/showcase/promptpanda.webp"
          title="Prompt Panda"
          description="AI-powered prompt management system designed to streamline and enhance the use of AI prompts."
        >
          <Tag
            text="ai-tools"
            className="border-green-600 bg-green-50 text-green-600"
          />
          <Tag
            text="saas"
            className="border-blue-500 bg-slate-50 text-blue-500"
          />
        </ShowcaseItem>

        <ShowcaseItem
          url="https://scribeist.com/"
          thumb="img/lp/showcase/scribeist.webp"
          title="Scribeist"
          description="Create and publish high-quality, SEO-optimized blog posts with AI."
        >
          <Tag
            text="marketing"
            className="border-red-600 bg-red-50 text-red-600"
          />
          <Tag
            text="saas"
            className="border-blue-500 bg-slate-50 text-blue-500"
          />
        </ShowcaseItem>
      </div>
    </SectionContainer>
  )
}

export default ShowcaseGallery
