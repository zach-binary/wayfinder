import './main.css'
const sweetVideo = require('./scan-demo.mov')
const Elm = require('./App.elm')

const root = document.getElementById('root')

Elm.App.embed(root, sweetVideo)

