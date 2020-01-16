{{={= =}=}}
import _ from 'lodash'
import React from 'react'

import FormControlLabel from '@material-ui/core/FormControlLabel'
import Switch from '@material-ui/core/Switch'
import Button from '@material-ui/core/Button'
import TextField from '@material-ui/core/TextField'

import {= entityClassName =} from '../{= entityClassName =}'


export default class {= entityForm.name =} extends React.Component {
  // TODO: Add propTypes.

  state = {
    // TODO(matija): Currently we hardcoded default values for each field type.
    // In the future we might let user decide on the default value.
    fields: {
      {=# entityTypedFields =}
      {=# boolean =}
      {= name =}: false,
      {=/ boolean =}
      {=# string =}
      {= name =}: '',
      {=/ string =}
      {=/ entityTypedFields =}
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
    {=# entityTypedFields =}
    {=# boolean =}
    this.setField('{= name =}', false)
    {=/ boolean =}
    {=# string =}
    this.setField('{= name =}', '')
    {=/ string =}
    {=/ entityTypedFields =}
  }

  toggleField = (name) => {
    this.setField(name, prevState => !prevState.fields[name])
  }

  getField = (name) => {
    return this.state.fields[name]
  }

  handleSubmit = () => {
    this.props.onCreate(new {= entityClassName =}(this.state.fields))
    this.resetAllFields()
  }

  render() {
    return (
      <div style={ { margin: '20px' } }>
        <form noValidate onSubmit={this.handleSubmit} action="javascript:void(0);">

          {=# entityTypedFields =}

          {=# boolean =}
           <div>
            <FormControlLabel
              label="{= name =}"
              control={
                <Switch
                  checked={this.getField('{= name =}')}
                  onChange={() => this.toggleField('{= name =}')}
                  value="{= name =}"
                />
              }
            />
          </div>
          {=/ boolean =}

          {=# string =}
          <div>
            <TextField
              label="{= name =}"
              value={this.getField('{= name =}')}
              onChange={event => this.setField('{= name =}', event.target.value)}
              margin="normal"
            />
          </div>
          {=/ string =}

          {=/ entityTypedFields =}

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
