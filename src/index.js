require('tachyons')
require('./styles.css')

const app = require('./Main.elm')['Elm']['Main']['init']({
  node: document.getElementById('elm'),
  flags: {
    now: Date.now(),
    window: { width: window.innerWidth, height: window.innerHeight },
    cache: localStorage.getItem("cache") || ""
  },
})

app.ports.cache.subscribe(function(string) {
  localStorage.setItem("cache", string)
})
