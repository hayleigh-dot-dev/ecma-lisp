const { Elm } = require('./src/elm')
const REPL = require('repl')

const lisp = Elm.Main.init()
// `write` is the callback provided to return something to the REPL
const send = (input, write) => {
  lisp.ports.console_o.subscribe(
    function recv ([ status, output ]) {
      lisp.ports.console_o.unsubscribe(recv)

      // It's a nodeback so the first parameter is `null` to represent no error.
      write(null, `${status} :|: ${output}`)
    } 
  )

  lisp.ports.console_i.send(input)
}

// Lines is used to store repl history while in multiline mode.
const lines = []
const repl = REPL.start({
  prompt: 'λ ',
  input: process.stdin,
  output: process.stdout,
  terminal: true,
  useColors: true,
  useGlobal: false,
  ignoreUndefined: true,
  preview: true,
  // This function is used to format REPL output just before we send it, in the
  // future we could do fancy things like colouring the output for error messages
  // and formatting type signatures.
  writer (output) {
    const [ status, result ] = output.split(' :|: ')

    repl.setPrompt('λ ')

    switch (status) {
      case 'success': 
        return result.split('\n').map(l => `    : ${l}`).join('\n')
      case 'failure': 
        return `    : <Error>`
    }
  },
  // This function is how we handle autocompletions in the REPL, a user can press
  // <tab> at any time to show possible autocompletions based on the current
  // input.
  completer (input) {
    const completions = 'add sub mul div .exit'.split(' ')
    const matches = completions.filter(c => c.startsWith(input))

    return [matches.length ? matches : completions, input]
  },
  // This is our custom eval function, it overrides the default node REPL
  // behaviour. This is what lets us evaluate lisp expressions instead of
  // javascript ones!
  eval (input, _, __, write) {
    if (input.endsWith('\\\n')) {
      lines.push(input.replace('\\\n', ''))

      repl.setPrompt('| ')
      repl.displayPrompt(false)
    } else if (lines.length == 0) {
      send(input.startsWith('(') ? input : `(${input})`, write)

    } else {
      input = lines.filter(l => l != '').join(' ') + ' ' + input
      input = input.trim()
      lines.length = 0

      send(input.startsWith('(') ? input : `(${input})`, write)
    }
  }
})
