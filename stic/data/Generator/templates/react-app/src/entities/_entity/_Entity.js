{{={= =}=}}
export default class {= entity.name =} {
  _data = {}

  constructor (data = {}) {
    this._data = {
      {=# entity.fields =}
      {= name =}: data.{= name =},
      {=/ entity.fields =}
    }
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
