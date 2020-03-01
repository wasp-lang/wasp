{{={= =}=}}
import uuidv4 from 'uuid/v4'

export default class {= entityClassName =} {
  _data = {}

  constructor (data = {}) {
    this._data = {
      id: data.id || uuidv4(),
      {=# entity.fields =}
      {= name =}: data.{= name =},
      {=/ entity.fields =}
    }
  }

  get id () {
    return this._data.id
  }

  {=# entity.fields =}
  get {= name =} () {
    return this._data.{= name =}
  }
  {=/ entity.fields =}

  toData () {
    return this._data
  }
}
