
import uuidv4 from 'uuid/v4'

export default class Task {
  _data = {}

  constructor (data = {}) {
    this._data = {
      id: data.id || uuidv4(),
      description: data.description,
      isDone: data.isDone,
    }
  }

  get id () {
    return this._data.id
  }

  get description () {
    return this._data.description
  }
  get isDone () {
    return this._data.isDone
  }

  toData () {
    return this._data
  }
}
