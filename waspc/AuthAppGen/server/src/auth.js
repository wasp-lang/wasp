

const auth = async (req, res, next) => {
  // Extract token from header and fetch user.

  console.log(req.get('Authorization'))

  next()
}

export default auth
