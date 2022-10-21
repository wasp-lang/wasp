import React, { useState, useRef, useContext } from 'react'
import { Plus, X, MoreHorizontal } from 'react-feather'
import { Popover } from 'react-tiny-popover'
import classnames from 'classnames'
import { DragDropContext, Droppable, Draggable } from 'react-beautiful-dnd'

import { useQuery } from '@wasp/queries'
import getListsAndCards from '@wasp/queries/getListsAndCards'
import createList from '@wasp/actions/createList'
import updateList from '@wasp/actions/updateList'
import deleteList from '@wasp/actions/deleteList'
import createListCopy from '@wasp/actions/createListCopy'

import createCard from '@wasp/actions/createCard'
import updateCard from '@wasp/actions/updateCard'

import UserPageLayout from './UserPageLayout'

import './Main.css'
import {
  calcNewPosOfDndItemInsertedInAnotherList,
  calcNewPosOfDndItemMovedWithinList,
  PositionContext,
  PositionProvider
} from './PositionContext'


const createListIdToSortedCardsMap = (listsAndCards) => {
  const listIdToSortedCardsMap = {}

  listsAndCards.forEach(list => {
    listIdToSortedCardsMap[list.id] = [...list.cards].sort((a, b) => a.pos - b.pos)
  })
  
  return listIdToSortedCardsMap
}

const MainPage = ({ user }) => {
  const { data: listsAndCards, isFetchingListsAndCards, errorListsAndCards }
    = useQuery(getListsAndCards)

  // NOTE(matija): this is only a shallow copy.
  const listsSortedByPos = listsAndCards && [...listsAndCards].sort((a, b) => a.pos - b.pos)

  // Create a map with listId -> cards sorted by pos.
  const listIdToSortedCardsMap = listsAndCards && createListIdToSortedCardsMap(listsAndCards)

  const onDragEnd = async (result) => {
    // Item was dropped outside of the droppable area.
    if (!result.destination) {
      return
    }

    // TODO(matija): make an enum for type strings (BOARD, CARD).
    if (result.type === 'BOARD') {
      const newPos =
        calcNewPosOfDndItemMovedWithinList(
          listsSortedByPos, result.source.index, result.destination.index
        )

      try {
        const movedListId = listsSortedByPos[result.source.index].id
        await updateList({ listId: movedListId, data: { pos: newPos } })
      } catch (err) {
        window.alert('Error while updating list position: ' + err.message)
      }
    } else if (result.type === 'CARD') {
      const sourceListId = result.source.droppableId
      const destListId = result.destination.droppableId
      // TODO(matija): this is not the nicest solution, we should have a consistent naming system
      // for draggable ids (for lists we put prefix in the id, while for cards we use 
      // their db id directly, because that saves us a bit of work in the further code.
      //
      // NOTE(matija): All draggable ids must be unique, even if they belong to different
      // droppable areas. This is why for lists we didn't use a db id directly, because it would
      // overlap with card ids. And for cards it was handy to have db id as draggable id, because
      // then we can easily access the data for the specific card.
      const movedCardId = Number(result.draggableId)

      const destListCardsSortedByPos = listIdToSortedCardsMap[destListId]

      let newPos = undefined
      if (sourceListId === destListId) { // Card got moved within the same list.
        newPos = calcNewPosOfDndItemMovedWithinList(
          destListCardsSortedByPos, result.source.index, result.destination.index
        )
      } else { // Card got inserted from another list.
        newPos = calcNewPosOfDndItemInsertedInAnotherList(
          destListCardsSortedByPos, result.destination.index
        )
      }

      try {
        await updateCard({ cardId: movedCardId, data: { pos: newPos, listId: destListId } })
      } catch (err) {
        window.alert('Error while updating card position: ' + err.message)
      }
    } else {
      // TODO(matija): throw error.
    }
  }

  return (
    <UserPageLayout user={user}>
      <div className='board-header'>
        <div className='board-name'>
          <h1 className='board-header-text'>Your board</h1>
        </div>
      </div>

      <DragDropContext onDragEnd={onDragEnd}>
        <Droppable droppableId="board" direction="horizontal" type="BOARD" >
          {(provided, snapshot) => (
            <PositionProvider items={listsSortedByPos}>
              <div id='board' className='u-fancy-scrollbar'
                ref={provided.innerRef}
                {...provided.droppableProps}
              >
                { listsSortedByPos && listIdToSortedCardsMap &&
                    <Lists
                      lists={listsSortedByPos}
                      listIdToCardsMap={listIdToSortedCardsMap}
                    />
                  }
                {provided.placeholder}
                <AddList />
              </div>
            </PositionProvider>
          )}
        </Droppable>
      </DragDropContext>

    </UserPageLayout>
  )
}

const Lists = ({ lists, listIdToCardsMap }) => {
    // TODO(matija): what if some of the props is empty? Although we make sure not to add it
    // to DOM in that case.

    return lists.map((list, index) => {
      return (
        <List list={list} key={list.id} index={index}
          cards={listIdToCardsMap[list.id]}
        />
      )
    }) 
}

const List = ({ list, index, cards }) => {
  const [isPopoverOpen, setIsPopoverOpen] = useState(false)
  const [isInEditMode, setIsInEditMode] = useState(false);
  const { posAfter } = useContext(PositionContext);

  const handleListNameUpdated = async (listId, newName) => {
    try {
      await updateList({ listId, data: { name: newName } })
    } catch (err) {
      window.alert('Error while updating list name: ' + err.message)
    }
  }

  const handleAddCard = async () => {
    setIsInEditMode(true);
    setIsPopoverOpen(false);
  }

  const handleCopyList = async (listId, idx) => {
    try {
      // pos is not easily calculated because
      await createListCopy({ listId, pos: posAfter(idx) });
    } catch (err) {
      window.alert('Error while copying list: ' + err.message)
    }

    setIsPopoverOpen(false);
  }

  const handleDeleteList = async (listId) => {
    try {
      await deleteList({ listId })
    } catch (err) {
      window.alert('Error while deleting list: ' + err.message)
    }
    setIsPopoverOpen(false)
  }

  const ListMenu = () => {
    return (
      <div className='popover-menu'>
        <div className='popover-header'>
          <div className='popover-header-item'>
            <button className='popover-header-close-btn dark-hover fake-invisible-item'>
              <X size={16}/>
            </button>
          </div>
          <span className='popover-header-title popover-header-item'>List&nbsp;actions</span>
          <div className='popover-header-item'>
            <button
              className='popover-header-close-btn dark-hover'
              onClick={() => setIsPopoverOpen(false)}
            >
              <X size={16}/>
            </button>
          </div>
        </div>
        <div className='popover-content'>
          <ul className='popover-content-list'>
            <li>
              <button onClick={() => handleAddCard()}>
                Add card...
              </button>
            </li>
            <li>
              <button onClick={() => handleCopyList(list.id, index)}>
                Copy list...
              </button>
            </li>
            <li>
              <button onClick={() => handleDeleteList(list.id)}>
                Delete this list
              </button>
            </li>
          </ul>
        </div>
      </div>
    )
  }

  return (
    <Draggable
      key={list.id}
      draggableId={`listDraggable-${list.id}`}
      index={index}
    >
      {(provided, snapshot) => (
        <div className='list-wrapper'
          ref={provided.innerRef}
          {...provided.draggableProps}
          {...provided.dragHandleProps}
        >
          <div className='list'>
            <div className='list-header'>
              <textarea
                className='list-header-name mod-list-name'
                onBlur={(e) => handleListNameUpdated(list.id, e.target.value)}
                defaultValue={ list.name }
              />
              <div className='list-header-extras'>
                <Popover
                  isOpen={isPopoverOpen}
                  onClickOutside={() => setIsPopoverOpen(false)}
                  positions={['bottom', 'right', 'left']}
                  align='start'
                  padding={6}
                  content={<ListMenu/>}
                >
                  <div
                    className='list-header-extras-menu dark-hover'
                    onClick={() => setIsPopoverOpen(!isPopoverOpen)}
                  >
                    <MoreHorizontal size={16}/>
                  </div>
                </Popover>
              </div>
            </div> {/* eof list-header */}

            <Droppable
              droppableId={`${list.id}`}
              direction="vertical"
              type="CARD"
            >
              {(provided, snapshot) => (
                <div className='cards'
                  ref={provided.innerRef}
                  {...provided.droppableProps}
                >
                  { cards && <Cards cards={cards} /> }
                  {provided.placeholder}
                </div>
              )}
            </Droppable>

            <div className='card-composer-container'>
              <PositionProvider items={cards}>
                <AddCard
                  listId={list.id}
                  isInEditMode={isInEditMode}
                  setIsInEditMode={setIsInEditMode}
                />
              </PositionProvider>
            </div>
          </div>
        </div>
      )}
    </Draggable>
  )
}

const Cards = ({ cards }) => {
  return (
    <div className='list-cards'>
      { cards.map((card, index) => <Card card={card} key={card.id} index={index} />) }
    </div>
  )
}

const Card = ({ card, index }) => {
  return (
    <Draggable
      key={card.id}
      draggableId={`${card.id}`}
      index={index}
    >
      {(provided, snapshot) => (
        <div className='list-card'
          ref={provided.innerRef}
          {...provided.draggableProps}
          {...provided.dragHandleProps}
        >
          <span className='list-card-title'>{ card.title }</span>
        </div>
      )}
    </Draggable>
  )
}

const AddList = () => {
  const [isInEditMode, setIsInEditMode] = useState(false)
  const { nextPos } = useContext(PositionContext)

  const AddListButton = () => {
    return (
      <button
        className='open-add-list'
        onClick={() => setIsInEditMode(true)}
      >
          <div className='add-icon'>
            <Plus size={16} strokeWidth={2} />
          </div>
          Add a list
      </button>
    )
  }

  const AddListInput = () => {
    const handleAddList = async (event) => {
      event.preventDefault()
      try {
        const listName = event.target.listName.value
        event.target.reset()
        await createList({ name: listName, pos: nextPos() })
      } catch (err) {
        window.alert('Error: ' + err.message)
      }
    }

    return (
      <form onSubmit={handleAddList}>
        <input
          className='list-name-input'
          autoFocus
          name='listName'
          type='text'
          defaultValue=''
          placeholder='Enter list title...'
        />
        <div className='list-add-controls'>
          <input className='list-add-button' type='submit' value='Add list' />
          <div
            className='list-cancel-edit'
            onClick={() => setIsInEditMode(false)}
          >
            <X/>
          </div>
        </div>
      </form>
    )
  }

  return (
    <div
      className={classnames(
        'add-list', 'list-wrapper', 'mod-add', { 'is-idle': !isInEditMode }
      )}
    >
      { isInEditMode ? <AddListInput /> : <AddListButton /> }
    </div>
  )
}

const AddCard = ({ listId, isInEditMode, setIsInEditMode }) => {

  const AddCardButton = () => {
    return (
      <button
        className='open-card-composer dark-hover'
        onClick={() => setIsInEditMode(true)}
      >
        <div className='add-icon'>
          <Plus size={16} strokeWidth={2} />
        </div>
        Add a card
      </button>
    )
  }

  const AddCardInput = ({ listId }) => {
    const formRef = useRef(null)
    const { nextPos } = useContext(PositionContext);

    const submitOnEnter = (e) => {
      if (e.keyCode === 13 /* && e.shiftKey == false */) {
        e.preventDefault()

        formRef.current.dispatchEvent(
          new Event('submit', { cancelable: true, bubbles: true })
        )
      }
    }

    const handleAddCard = async (event, listId) => {
      event.preventDefault()
      try {
        const cardTitle = event.target.cardTitle.value
        event.target.reset()
        await createCard({ title: cardTitle, pos: nextPos(), listId })
      } catch (err) {
        window.alert('Error: ' + err.message)
      }
    }

    return (
      <form className='card-composer' ref={formRef} onSubmit={(e) => handleAddCard(e, listId)}>
        <div className='list-card'>
          <textarea
            className='card-composer-textarea'
            onKeyDown={submitOnEnter}
            autoFocus
            name='cardTitle'
            placeholder='Enter a title for this card...'
          />
        </div>
        <div className='card-add-controls'>
          <input className='card-add-button' type='submit' value='Add card' />
          <div
            className='card-cancel-edit'
            onClick={() => setIsInEditMode(false)}
          >
            <X/>
          </div>
        </div>
      </form>
    )
  }
  return (
    <div>
      { isInEditMode ? <AddCardInput listId={listId} /> : <AddCardButton /> }
    </div>
  )
}

export default MainPage
