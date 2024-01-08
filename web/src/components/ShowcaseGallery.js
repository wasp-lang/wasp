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
              relative mb-4 h-60 w-full overflow-auto overflow-y-hidden
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
          url="/blog/2022/10/28/farnance-hackathon-winner"
          thumb="img/lp/showcase/farnance-dashboard.png"
          title="Farnance: SaaS marketplace for farmers"
          description="See how Julian won HackLBS 2021 among 250 participants and why Wasp was instrumental for the team's victory."
        >
          <Tag
            text="hackathon"
            className="border-yellow-600 bg-yellow-50 text-yellow-600"
          />
          <Tag
            text="material-ui"
            className="border-blue-500 bg-slate-50 text-blue-500"
          />
        </ShowcaseItem>

        <ShowcaseItem
          url="/blog/2022/11/26/michael-curry-usecase"
          thumb="img/lp/showcase/grabbit-hero.png"
          title="Grabbit: Easily manage dev environments"
          description="See how Michael built and deployed an internal tool for managing dev resources at StudentBeans."
        >
          <Tag
            text="internal-tools"
            className="border-green-600 bg-green-50 text-green-600"
          />
        </ShowcaseItem>

        <ShowcaseItem
          url="/blog/2022/11/26/erlis-amicus-usecase"
          thumb="img/lp/showcase/amicus-landing.png"
          title="Amicus: Task and workflow management for legal teams"
          description="See how Erlis rolled out fully-fledged SaaS as a team of one in record time and got first paying customers."
        >
          <Tag
            text="startup"
            className="border-fuchsia-600 bg-fuchsia-50 text-fuchsia-600"
          />
          <Tag
            text="material-ui"
            className="border-blue-500 bg-slate-50 text-blue-500"
          />
        </ShowcaseItem>
      </div>
    </SectionContainer>
  )
}

export default ShowcaseGallery
