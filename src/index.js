import './main.css'
const mapPath = require('./map.jpg')
const Elm = require('./App.elm')

const root = document.getElementById('root')

Elm.App.embed(root, mapPath)

setTimeout(function() {
    document.querySelector('svg').addEventListener('click', function(e) {
        console.log(e.offsetX, e.offsetY);
    });
}, 1000);
