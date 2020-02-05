{{={= =}=}}
import _ from 'lodash'
import React from 'react'

import FormControlLabel from '@material-ui/core/FormControlLabel'
import Switch from '@material-ui/core/Switch'
import Button from '@material-ui/core/Button'
import TextField from '@material-ui/core/TextField'

import {= entityClassName =} from '../{= entityClassName =}'


export default class {= formName =} extends React.Component {
  // TODO: Add propTypes.

  state = {
    fields: {
      {=# formFields =}
      {=# boolean =}
      {= name =}: {= defaultValue =},
      {=/ boolean =}
      {=# string =}
      {= name =}: '{= defaultValue =}',
      {=/ string =}
      {=/ formFields =}
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
    {=# formFields =}
    {=# boolean =}
    this.setField('{= name =}', {= defaultValue =})
    {=/ boolean =}
    {=# string =}
    this.setField('{= name =}', '{= defaultValue =}')
    {=/ string =}
    {=/ formFields =}
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
      <div className={this.props.className}>
        <form noValidate onSubmit={this.handleSubmit} action="javascript:void(0);">

          {=# formFields =}

          {=# boolean =}
          {=# show =}
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
          {=/ show =}
          {=/ boolean =}

          {=# string =}
          {=# show =}
          <div>
            <TextField
              label="{= name =}"
              {=# placeholder =}
              placeholder="{= placeholder =}"
              {=/ placeholder =}
              value={this.getField('{= name =}')}
              onChange={event => this.setField('{= name =}', event.target.value)}
              margin="normal"
              fullWidth
              InputLabelProps={{
                shrink: true
              }}
            />
          </div>
          {=/ show =}
          {=/ string =}

          {=/ formFields =}

          {=# showSubmitButton =}
          <div>
            <Button type="submit" variant="contained" color="primary">
              {this.props.submitButtonLabel || 'Submit'}
            </Button>
          </div>
          {=/ showSubmitButton =}
        </form>
      </div>
    )
  }
}
