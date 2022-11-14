
const express = require('express')
const fs = require('fs')
const bodyParser = require('body-parser');

const port = 3000

// Cette ligne indique le répertoire qui contient
// les fichiers statiques: html, css, js, images etc.

const app = express()

// set the view engine to ejs
app.set('view engine', 'ejs');

app.use(express.static('public/'))

app.use(express.urlencoded({ extended: true }))

app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: true })); 

app.get('/', (req, res) => {
  let data = fs.readFileSync('data/data.json')
  data = JSON.parse(data)
  res.render('index', { data })
})

app.listen(port, () => {
  console.log(`Example app listening at http://localhost:${port}`)
})
