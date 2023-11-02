import React from 'react'
import Link from '@docusaurus/Link'
import { BlogPostProvider } from '@docusaurus/theme-common/internal'
import BlogPostItem from '@theme/BlogPostItem'

import { ArrowRight } from 'react-feather'

const getReadingTimeStatement = (readingTimeFloat) =>
  Math.ceil(readingTimeFloat) + ' min read'

export default function BlogPostItems({
  items,
  component: BlogPostItemComponent = BlogPostItem,
}) {
  // TODO(matija): what if there are no items (blog posts)?
  const featuredItem = items[0]
  const coverImgSrc =
    featuredItem.content.frontMatter.image || 'img/blog-default-img.png'

  return (
    <>
      <section
        className={`
          bg-yellow-500 py-20 pb-20
          md:py-36
        `}
      >
        <div
          className={`
            container mx-auto
          `}
        >
          <article
            className={`
              relative grid-cols-2 items-stretch gap-5 md:px-20
              lg:grid
              lg:px-24
            `}
          >
            {/* Content - left col */}
            <div className="self-center lg:pr-16">
              <span
                className={`
                  mb-6 flex items-center
                  justify-between text-[11px] font-semibold
                  uppercase tracking-wider
                  text-neutral-600
                `}
              >
                <span>By {featuredItem.content.metadata.authors[0].name}</span>
                <div className="mx-2 h-px flex-1 bg-neutral-600/20"></div>
                <span>
                  {getReadingTimeStatement(
                    featuredItem.content.metadata.readingTime
                  )}
                </span>
              </span>

              {/* Title */}
              <h1 className="text-4xl leading-tight text-neutral-800">
                {featuredItem.content.frontMatter.title}
              </h1>

              {/* Read more button */}
              <div className="mt-24">
                <span
                  className={`
                    rounded-md bg-neutral-600
                    px-3 py-1.5 text-white
                  `}
                >
                  <div className="group inline-flex items-center gap-1">
                    <span>Read more</span>
                    <span className="text-yellow-400">→</span>
                  </div>
                </span>
              </div>
            </div>

            {/* Image - right col */}
            <img
              src={coverImgSrc}
              className={`
                  mb-6
                  hidden rounded-2xl
                  object-cover lg:flex
                `}
            />

            <Link
              to={featuredItem.content.metadata.permalink}
              className="absolute inset-0 opacity-0"
            />
          </article>
        </div>
      </section>

      <section className="container mx-auto px-4 sm:px-6 md:px-20 lg:px-28">
        <div className="mb-8 grid grid-cols-1 gap-5 md:-mt-12 md:grid-cols-2 xl:grid-cols-3">
          {items.slice(1).map((item) => {
            const BlogPostContent = item.content
            const coverImgSrc =
              item.content.frontMatter.image || 'img/blog-default-img.png'

            return (
              <article
                key={item.content.metadata.permalink}
                className={`
                  relative
                  flex

                  flex-col
                  rounded-2xl
                  border border-neutral-200/20
                  bg-[--custom-blog-card-background-color] p-8

                  shadow-sm shadow-yellow-500/25
                  transition-all hover:shadow-lg
                  hover:shadow-yellow-500/25
                `}
              >
                {/* Image */}
                <img
                  src={coverImgSrc}
                  className={`
                      mb-6
                      h-64 rounded-xl
                      object-cover md:-mx-6 md:-mt-6 md:max-w-none
                    `}
                />

                {/* Written by + time to read */}
                <span
                  className={`
                    flex items-center
                    text-[11px] font-semibold uppercase
                    tracking-wider text-[--custom-blog-card-timestamp-color]
                  `}
                >
                  <span className="truncate">
                    By {item.content.metadata.authors[0].name}
                  </span>
                  <div className="mx-2 h-px flex-1 bg-gray-200"></div>
                  <span>
                    {getReadingTimeStatement(item.content.metadata.readingTime)}
                  </span>
                </span>

                {/* Title */}
                <h1 className="my-4 h-28 text-xl">
                  {item.content.frontMatter.title}
                </h1>

                <p className="mb-0 mt-0 font-semibold text-yellow-600">
                  Read more →
                </p>

                <Link
                  to={item.content.metadata.permalink}
                  className="absolute inset-0 opacity-0"
                />
              </article>
            )

            /* This is what it looked like right after swizzle --eject.
            return (
              <BlogPostProvider
                key={BlogPostContent.metadata.permalink}
                content={BlogPostContent}>
                <BlogPostItemComponent>
                  <BlogPostContent />
                </BlogPostItemComponent>
              </BlogPostProvider>
            )
            */
          })}
        </div>
      </section>
    </>
  )
}
