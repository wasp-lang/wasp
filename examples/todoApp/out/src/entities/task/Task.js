import uuidv4 from 'uuid/v4'

export default class Task {
  _data = {}

  constructor (data = {}) {
    this._data = {
      id: data.id || uuidv4(),
      isDone: data.isDone,
      description: data.description,
    }
  }

  get id () {
    return this._data.id
  }

  get isDone () {
    return this._data.isDone
  }
  get description () {
    return this._data.description
  }

  toData () {
    return this._data
  }
}
