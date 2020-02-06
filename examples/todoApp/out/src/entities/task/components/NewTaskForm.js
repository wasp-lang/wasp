import _ from 'lodash'
import React from 'react'
import PropTypes from 'prop-types'

import FormControlLabel from '@material-ui/core/FormControlLabel'
import Switch from '@material-ui/core/Switch'
import Button from '@material-ui/core/Button'
import TextField from '@material-ui/core/TextField'

import Task from '../Task'


export default class NewTaskForm extends React.Component {
  static propTypes = {
    onCreate: PropTypes.func,
    submitButtonLabel: PropTypes.string
  }

  state = {
    fields: {
      isDone: false,
      description: '',
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
    this.setField('isDone', false)
    this.setField('description', '')
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
      <div className={this.props.className}>
        <form noValidate onSubmit={this.handleSubmit} action="javascript:void(0);">
          <div>
            <TextField
              placeholder="What needs to be done?"
              value={this.getField('description')}
              onChange={event => this.setField('description', event.target.value)}
              margin="normal"
              fullWidth
              InputLabelProps={{
                shrink: true
              }}
            />
          </div>
        </form>
      </div>
    )
  }
}
