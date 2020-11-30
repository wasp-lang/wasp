import React, { useState } from 'react'
import PropTypes from 'prop-types'

import { useQuery } from '@wasp/queries'

import ArticleList from './ArticleList'

const ArticleListPaginated = (props) => {
  // NOTE: Different approach to externalizing this pagination logic would be to
  //   pass `articles`, `count` and `refetch` as props, where `refetch` is a
  //   "callback" function which would take `skip` and `take` args and then appropriately
  //   set `articles and `count`.
  //   This way this component would be somewhat dumber and would be making less assumptions
  //   about where the data is coming from, therefore making it more flexible.
  //   However, that also means implementing more logic outside of this component.
  //   If we will try to generalize this as a mechanism in Wasp, it might be interesting
  //   to offer both types of interface, one as lower lvl interface and another as higher lvl interface.

  const pageSize = props.pageSize || 10

  const [pageIdx, setPageIdx] = useState(0)
  const { data: articlesData } = useQuery(
    props.query,
    props.makeQueryArgs({ skip: pageIdx * pageSize, take: pageSize }),
    { keepPreviousData: true }
  )

  const articles = articlesData?.articles
  const articlesCount = articlesData?.count
  const pageCount = Math.trunc(articlesCount / pageSize)

  const goToPrevPage  = () => setPageIdx(Math.max(pageIdx - 1, 0))
  const goToNextPage  = () => setPageIdx(Math.min(pageIdx + 1, pageCount - 1))
  const goToLastPage  = () => setPageIdx(pageCount - 1)
  const goToFirstPage = () => setPageIdx(0)

  return (
    <div>
      <ArticleList articles={articles || []} />

      { pageCount > 1 && (
        <div>
          { pageIdx > 0 && (
            <>
              <button onClick={ goToFirstPage }> 1 </button>
              <button onClick={ goToPrevPage }> &lt; </button>
            </>
          ) }

          { /* TODO: Make the current page number an input which user can change. */ }
          { pageIdx + 1 }

          { pageIdx < pageCount - 1 && (
            <>
              <button onClick={ goToNextPage }> &gt; </button>
              <button onClick={ goToLastPage }> { pageCount } </button>
            </>
          )}
        </div>
      ) }
    </div>
  )
}
ArticleListPaginated.propTypes = {
  query: PropTypes.func.isRequired, // NOTE: Should return .articles and .count .
  makeQueryArgs: PropTypes.func.isRequired, // NOTE: Receives { skip, take } as an argument.
  pageSize: PropTypes.number
}

export default ArticleListPaginated
