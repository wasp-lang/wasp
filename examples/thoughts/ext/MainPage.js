import React, { useState, useRef } from 'react'
import ReactMarkdown from 'react-markdown'
import styled from 'styled-components'
import { useHistory } from 'react-router-dom'

import TagsSidebar from './TagsSidebar.js'
import TopNavbar from './TopNavbar.js'
import { getTagColor } from './tag.js'

import createThought from '@wasp/actions/createThought'

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

const StyledMainPage = styled.div`
  display: flex;
  flex-direction: column;
  align-items: center;
`

const MainContainer = styled.div`
  width: 100%;
  display: flex;
  flex-direction: row;
  align-items: flex-start;
`

const MainPage = ({ user }) => {
  // TODO: Remove duplication! layout, navbar, sidebar, ...
  return (
    <StyledMainPage>
      <TopNavbar user={user} />

      <MainContainer>
        <TagsSidebar />
        <Thought />
      </MainContainer>
    </StyledMainPage>
  )
}

const StyledThought = styled.div`
  min-height: 90vh;
  margin-right: 300px;
  width: 100%;
  display: flex;
  flex-direction: column;
  align-items: center;
`

const ThoughtForm = styled.form`
  display: flex;
  flex-direction: column;
  margin-top: 50px;
  margin-bottom: 50px;
`

const ThoughtTags = styled.div`
  display: flex;
  flex-wrap: wrap;
  flex-flow: flex-start;
  padding: 10px;
`

const ThoughtTag = styled.div`
  margin: 0px 10px;
  &:hover {
    text-decoration: line-through;
    cursor: pointer;
  }
  &:before {
    text-decoration: line-through;
    cursor: pointer;
  }
`

const ThoughtPreview = styled.div`
  width: 800px;
  height: 66vh;
  font-size: 20px;
  border: 0px;
  border-radius: 2px;
  box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
  resize: none;
  margin-bottom: 10px;
  padding: 20px;
`

const ThoughtEditor = styled.textarea`
  width: 800px;
  height: 66vh;
  font-size: 20px;
  border: 0px;
  border-radius: 2px;
  box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
  resize: none;
  margin-bottom: 10px;
  padding: 20px;
  &:focus {
    outline: none;
  }
`

const ThougtNewTag = styled.span`
  color: grey;
  margin-left: 10px;
`

const ThoughtNewInput = styled.input`
  border: 0px;
  font: inherit;
  &:focus {
    outline: none;
  }
`

const ThoughtButtons = styled.div`
  display: flex;
  flex-direction: row;
  justify-content: flex-end;
`

const PlainButton = styled.button`
  border: none;
  outline: none;
  padding: 0;
  color: inherit;
  background: none;
  font: inherit;
  text-decoration: underline;
  &:hover {
    cursor: pointer;
  }
`

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

  const resetForm = () => {
    setTextMd(defaultTextMd)
    setTagNames(defaultTagNames)
    setNewTagName(defaultNewTagName)
    setInPreviewMode(defaultInPreviewMode)
  }

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
        resetForm()
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
    <StyledThought>
      <ThoughtForm ref={formRef}>
        <ThoughtTags>
          { tagNames.map(tagName => (
            <ThoughtTag
                 onClick={() => removeTag(tagName)}
                 key={tagName}
                 style={{ color: getTagColor(tagName) }}>
              { tagName }
            </ThoughtTag>
          ))}
          <ThougtNewTag>
            #
            <ThoughtNewInput
              type="text" value={newTagName} onChange={e => setNewTagNameIfValid(e.target.value)}
              onKeyDown={handleTagsKeyPress}
              onBlur={handleTagsBlur}
              placeholder="add.tags.here..."
            />
          </ThougtNewTag>
        </ThoughtTags>
        <div>
          { inPreviewMode
            ? <ThoughtPreview>
                <ReactMarkdown children={textMd} />
              </ThoughtPreview>
            : <ThoughtEditor
                value={textMd}
                onChange={e => setTextMd(e.target.value)}
                onKeyDown={handleThoughtEditorKeyPress}
                placeholder="Write here (Markdown supported, Alt+Enter to submit) ..."
                autoFocus={true}
              />
          }
        </div>
        <ThoughtButtons>
          <PlainButton
                  onClick={e => { e.preventDefault(); toggleInPreviewMode() }}>
            { inPreviewMode ? 'edit' : 'preview' }
          </PlainButton>
          &nbsp;|&nbsp;
          <PlainButton onClick={submit}> submit </PlainButton>
        </ThoughtButtons>
      </ThoughtForm>
    </StyledThought>
  )
}

export default MainPage
