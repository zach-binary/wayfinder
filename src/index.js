import './main.css'
const mapPath = require('./map.jpg')
const Elm = require('./App.elm')

const root = document.getElementById('root')

Elm.App.embed(root, mapPath)

