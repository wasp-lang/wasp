import _ from 'lodash'
import React from 'react'

import FormControlLabel from '@material-ui/core/FormControlLabel'
import Switch from '@material-ui/core/Switch'
import Button from '@material-ui/core/Button'
import TextField from '@material-ui/core/TextField'

import Task from '../Task'


export default class NewTaskForm extends React.Component {
  // TODO: Add propTypes.

  state = {
    fields: {
      description: '',
      isDone: false,
    }
  }

  setField = (name, valueOrFn) => {
    this.setState(prevState => ({
      fields: {
        ...prevState.fields,
        [name]: _.isFunction(valueOrFn) ? valueOrFn(prevState) : valueOrFn
      }
    }))
  }

  resetAllFields = () => {
    this.setField('description', '')
    this.setField('isDone', false)
  }

  toggleField = (name) => {
    this.setField(name, prevState => !prevState.fields[name])
  }

  getField = (name) => {
    return this.state.fields[name]
  }

  handleSubmit = () => {
    this.props.onCreate(new Task(this.state.fields))
    this.resetAllFields()
  }

  render() {
    return (
      <div style={ { margin: '20px' } }>
        <form noValidate onSubmit={this.handleSubmit} action="javascript:void(0);">



          <div>
            <TextField
              label="description"
              value={this.getField('description')}
              onChange={event => this.setField('description', event.target.value)}
              margin="normal"
            />
          </div>





        </form>
      </div>
    )
  }
}
