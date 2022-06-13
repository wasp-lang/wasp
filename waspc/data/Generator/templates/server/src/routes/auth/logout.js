import { handleRejection } from '../../utils.js'

export default handleRejection(async (req, res, next) => {
  // Clear the user from the session object and save.
  // This will ensure that re-using the old session id
  // does not have a logged in user. Ref: Express docs.
  req.session.user_id = null
  req.session.save(function (err) {
    if (err) next(err)

    // Regenerate the session, which is good practice to help
    // guard against forms of session fixation.
    req.session.regenerate(function (err) {
      if (err) next(err)
      return res.status(200).send()
    })
  })
})
