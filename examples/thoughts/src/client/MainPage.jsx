import React, { useState, useRef } from 'react'
import ReactMarkdown from 'react-markdown'
import { useHistory } from 'react-router-dom'
import Tag from './Tag'

import './Main.css'
import './Thought.css'

import createThought from '@wasp/actions/createThought'
import Layout from './Layout'

// TODO:
//   - Rename this file to Thought.js.
//   - Allow editing of existing thought.
//   - Allow deleting thoughts.
//   - Allow renaming tags.
//   - Implement pagination.
//   - When listing thoughts, show only first couple of lines (or just first line).
//   - Sort tags by the number of thoughts under them, descending?
//   - Implement searching/filtering through tags.
//   - Implement searching through the thoughts text.
//   - Allow hierarhical tags -> tags with '.' in them are "magical".
//     So, if Thought has #haskell and #haskell.exceptions tags, only #haskell tag
//     will be visible on the left side, while #haskell.exceptions tag will be shown as an "exceptions" tag
//     under the #haskell tag. Maybe there are some other smart ways of using this property.
//   - Set favicon.
//   - Support sharing thoughts (making them public). Not sure how this would go.
//   - Refactor and improve code.

const MainPage = ({ user }) => {
  return (
    <Layout user={user}>
      <Thought />
    </Layout>
  )
}

const Thought = (props) => {
  const defaultTextMd = ''
  const defaultNewTagName = ''
  const defaultTagNames = []
  const defaultInPreviewMode = false
  const [textMd, setTextMd] = useState(defaultTextMd)
  const [tagNames, setTagNames] = useState(defaultTagNames)
  const [newTagName, setNewTagName] = useState(defaultNewTagName)
  const [inPreviewMode, setInPreviewMode] = useState(defaultInPreviewMode)
  const history = useHistory()
  const formRef = useRef(null) // TODO: Why do I have this ref? I don't seem to use it anywhere?

  const setNewTagNameIfValid = (tagName) => {
    if (!tagName || /^[a-z](\.?[a-z0-9])*\.?$/.test(tagName)) {
      setNewTagName(tagName)
    }
  }

  const submit = async (e) => {
    e?.preventDefault()
    if (textMd.trim()) {
      if (!tagNames?.length) {
        return window.alert('You need to define at least one tag!')
      }
      try {
        await createThought({ textMarkdown: textMd.trim(), tagNames })
        history.push('/thoughts') // TODO: Would be cool if this was type checked somehow or if string was coming from the Wasp API.
      } catch (err) {
        return window.alert('Error: ' + err.message)
      }
    }
  }

  const toggleInPreviewMode = () => {
    setInPreviewMode(!inPreviewMode)
  }

  const handleThoughtEditorKeyPress = async (e) => {
    if (e.key === 'Enter' && e.altKey) {
      await submit(e)
    }
  }

  const addNewTag = () => {
    if (!newTagName) return
    console.log(newTagName)
    const tagNameToAdd = newTagName.replace(/\.$/, '')
    if (!tagNames.includes(tagNameToAdd)) {
      setTagNames([...tagNames, tagNameToAdd])
      setNewTagName('')
    }
  }

  const removeTag = (tagName) => {
    setTagNames(tagNames.filter(name => name !== tagName))
  }

  const handleTagsKeyPress = async (e) => {
    if (e.key === 'Enter') {
      e.preventDefault()
      addNewTag()
    }
  }

  const handleTagsBlur = async (e) => {
    addNewTag()
  }

  return (
    <div className="thought">
      <form ref={formRef}>

        <div className="thought-tags">
          { tagNames.map(tagName => (
            <Tag
              key={tagName}
              name={tagName}
              onClick={() => removeTag(tagName)}
              isDeletable={true}
            />
          ))}
          <span className="thought-tags-new">
            #
            <input
              type="text" value={newTagName} onChange={e => setNewTagNameIfValid(e.target.value)}
              onKeyDown={handleTagsKeyPress}
              onBlur={handleTagsBlur}
              placeholder="add.tags.here..."
            />
          </span>
        </div>

        <div className="thought-text">
          { inPreviewMode
            ? <div className="thought-preview">
                <ReactMarkdown children={textMd} />
              </div>
            : <div className="thought-editor">
                <textarea
                  value={textMd}
                  onChange={e => setTextMd(e.target.value)}
                  onKeyDown={handleThoughtEditorKeyPress}
                  placeholder="Write here (Markdown supported, Alt+Enter to submit) ..."
                  autoFocus={true}
                />
              </div>
          }
        </div>

        <div className="thought-buttons">
          <button className="plain"
                  onClick={e => { e.preventDefault(); toggleInPreviewMode() }}>
            { inPreviewMode ? 'edit' : 'preview' }
          </button>
          &nbsp;|&nbsp;
          <button className="plain" onClick={submit}> submit </button>
        </div>

      </form>
    </div>
  )
}

export default MainPage
