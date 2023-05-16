const express = require('express');

const app = express()

app.listen(8004)

app.get('/', (req, res) => {
  res.send({
    hello: 'world'
  })
})

app.get('/error', (req, res) => {
  res.status = 500
  res.send({
    error: 'Something not cool happened'
  })
})

app.get('/:toto', (req, res) => {
  res.status = 200
  res.send({
    p: req.params.toto
  })
})
