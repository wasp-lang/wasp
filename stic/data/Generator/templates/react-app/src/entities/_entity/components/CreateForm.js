{{={= =}=}}
import _ from 'lodash'
import React from 'react'

import FormControlLabel from '@material-ui/core/FormControlLabel'
import Switch from '@material-ui/core/Switch'
import Button from '@material-ui/core/Button'
import TextField from '@material-ui/core/TextField'

import {= entityClassName =} from '../{= entityClassName =}'


export default class CreateForm extends React.Component {
  // TODO: Add propTypes.

  state = {
    // TODO: Set to default values, not just undefined?
    //   How would we define these default values?
    //   Or maybe undefined is fine? How would this play with validation?
    fields: {
      {=# entity.fields =}
      {= name =}: undefined,
      {=/ entity.fields =}
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

  toggleField = (name) => {
    this.setField(name, prevState => !prevState.fields[name])
  }

  getField = (name) => {
    return this.state.fields[name]
  }

  handleSubmit = () => {
    this.props.onCreate(new {= entityClassName =}(this.state.fields))
  }

  render() {
    return (
      <div style={ { margin: '20px' } }>
        <form noValidate onSubmit={this.handleSubmit} action="javascript:void(0);">

          {=# entity.fields =}
          <div>
            {= name =}
          </div>

          {/* TODO: Generate appropriate code regarding on field type. How will I do this? Partials?
                I have to figure out how to do this in mustache.
          */}
          {/*
           <div>
            <FormControlLabel
              label="isDone"
              control={
                <Switch
                  checked={this.getField('isDone')}
                  onChange={() => this.toggleField('isDone')}
                  value="isDone"
                />
              }
            />
          </div>

          <div>
            <TextField
              label="Description"
              value={this.getField('description')}
              onChange={event => this.setField('description', event.target.value)}
              margin="normal"
            />
          </div>
          */}

          {=/ entity.fields =}

          <div>
            <Button type="submit" variant="contained" color="primary">
              {this.props.submitButtonLabel || 'Submit'}
            </Button>
          </div>
        </form>
      </div>
    )
  }
}
