import React from 'react';
import Link from '@docusaurus/Link'
import {BlogPostProvider} from '@docusaurus/theme-common/internal';
import BlogPostItem from '@theme/BlogPostItem';

import { ArrowRight } from 'react-feather'

const getReadingTimeStatement = (readingTimeFloat) => Math.ceil(readingTimeFloat) + ' min read'

export default function BlogPostItems({
  items,
  component: BlogPostItemComponent = BlogPostItem,
}) {

  // TODO(matija): what if there are no items (blog posts)?
  const featuredItem = items[0]
  const coverImgSrc = featuredItem.content.frontMatter.image || 'img/blog-default-img.png'

  return (
    <>
      <section
        className={`
          pb-20 py-20 md:py-36
          bg-yellow-500
        `}
      >
        <div
          className={`
            container mx-auto
          `}
        >
          <article
            className={`
              lg:grid grid-cols-2 gap-5 md:px-20 lg:px-24
              items-stretch
              relative
            `}
          >
            {/* Content - left col */}
            <div className='self-center lg:pr-16'>
              <span
                className={`
                  flex items-center justify-between
                  uppercase text-[11px] text-neutral-600
                  font-semibold tracking-wider
                  mb-6
                `}
              >
                <span>By {featuredItem.content.metadata.authors[0].name}</span>
                <div className='flex-1 bg-neutral-600/20 h-px mx-2'></div>
                <span>{getReadingTimeStatement(featuredItem.content.metadata.readingTime)}</span>
              </span>

              {/* Title */}
              <h1 className='text-4xl leading-tight text-neutral-800'>
                {featuredItem.content.frontMatter.title}
              </h1>


              {/* Read more button */}
              <div className='mt-24'>
                <span className={`
                    bg-neutral-600 text-white
                    px-3 py-1.5 rounded-md
                  `}
                >
                  <div className='group inline-flex gap-1 items-center'>
                    <span>Read more</span>
                    <span className='text-yellow-400'>→</span>
                  </div>
                </span>
              </div>

            </div>

            {/* Image - right col */}
            <img 
              src={coverImgSrc}
                className={`
                  object-cover
                  rounded-2xl mb-6
                  hidden lg:flex
                `}
            />

            <Link to={featuredItem.content.metadata.permalink} className='opacity-0 absolute inset-0'/>
          </article>
        </div>


      </section>

      <section className='container mx-auto px-4 sm:px-6 md:px-20 lg:px-28'>
        <div className='grid grid-cols-1 md:grid-cols-2 xl:grid-cols-3 gap-5 mb-8 md:-mt-12'>
          {items.slice(1).map((item) => {
            const BlogPostContent = item.content
            const coverImgSrc = item.content.frontMatter.image || 'img/blog-default-img.png'

            return (
              <article
                key={item.content.metadata.permalink}
                className={`
                  relative
                  rounded-2xl

                  bg-[--custom-blog-card-background-color]
                  transition-all
                  shadow-sm shadow-yellow-500/25
                  hover:shadow-lg hover:shadow-yellow-500/25

                  border border-neutral-200/20
                  flex flex-col
                  p-8
                `}
              >
                {/* Image */}
                <img 
                  src={coverImgSrc}
                    className={`
                      object-cover
                      rounded-xl mb-6
                      md:max-w-none h-64 md:-mx-6 md:-mt-6
                    `}
                />

                {/* Written by + time to read */}
                <span
                  className={`
                    flex items-center
                    uppercase text-[11px] text-[--custom-blog-card-timestamp-color]
                    font-semibold tracking-wider
                  `}
                >
                  <span className='truncate'>By {item.content.metadata.authors[0].name}</span>
                  <div className='flex-1 bg-gray-200 h-px mx-2'></div>
                  <span>{getReadingTimeStatement(item.content.metadata.readingTime)}</span>
                </span>

                {/* Title */}
                <h1 className='text-xl h-28 my-4'>
                  {item.content.frontMatter.title}
                </h1>

                <p className='text-yellow-600 font-semibold mt-0 mb-0'>Read more →</p>

                <Link to={item.content.metadata.permalink} className='opacity-0 absolute inset-0'/>
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
  );
}
