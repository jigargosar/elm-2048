import { hasPath, pathOr, zipObj } from 'ramda'

require('./styles.css')
require('tachyons')

function Cache(keys) {
  return {
    onCacheKV: ([key, val]) => {
      if (keys.includes(key)) {
        localStorage.setItem(key, JSON.stringify(val))
      } else {
        console.error('Invalid Cache Key:', key, 'validKeys:', keys)
      }
    },
    getAll: () => {
      const getParsed = key => parseTruthyOrNull(localStorage.getItem(key))
      return zipObj(keys, keys.map(getParsed))
    },
  }
}

{
  const cache = Cache([])
  const [app, subscribe] = initElmModuleWithPortHelpers(
    {
      node: document.getElementById('root'),
      flags: {
        now: Date.now(),
        viewSize: [window.innerWidth, window.innerHeight],
        scrollbarSize: [
          window.innerWidth - document.body.clientWidth,
          window.innerHeight - document.body.clientHeight,
        ],
        cache: cache.getAll(),
      },
    },
    require('./Main.elm'),
  )

  if (hasPath(['ports', 'cacheKV', 'subscribe'], app)) {
    subscribe('cacheKV', cache.onCacheKV)
  }
}

function initElmModule(initParams, module) {
  const elmObject = module['Elm']
  const mainModuleName = Object.getOwnPropertyNames(elmObject)[0]
  const initFn = elmObject[mainModuleName]['init']
  return initFn(initParams)
}

function initElmModuleWithPortHelpers(initParams, module) {
  const app = initElmModule(initParams, module)
  function subscribe(portName, callback) {
    pathOr(() => console.error(`${portName}.subscribe Port Not Found`), [
      'ports',
      portName,
      'subscribe',
    ])(app)(callback)
  }

  return [app, subscribe]
}

function parseTruthyOrNull(str) {
  try {
    return JSON.parse(str) || null
  } catch (e) {
    return null
  }
}

