import std/
  os,
  strutils

import errors
import interpreter
import parser
import scanner
import types

proc runFile(file: string) =
  ## Interpret a single file.
  var
    fileContents = file.readFile
    scanner = newScanner(fileContents)
    parser = newParser(scanner.scanTokens())

  let statements = parser.parse()
  interpret statements

  if hadError:
    quit 65

  if hadRuntimeError:
    quit 70

proc runPrompt =
  ## Run an interactive REPL.
  while true:
    stdout.write "> "
    stdout.flushFile

    let input = readLine stdin
    if input.len == 0:
      break

    try:
      var
        # Make the trailing semicolon optional in the REPL
        code = if input.endsWith(";"):
          input
        else:
          input & ";"
        scanner = newScanner(code)
        tokens = scanner.scanTokens()
        parser = newParser(tokens)

      let statements = parser.parse()

      #echo statements

      # If the last statement is an expression, print the value of the expression after
      # executing everything.
      if statements.len > 0 and statements[statements.high].kind == sExpression:
        interpret statements[0 ..< statements.high]

        let
          expr = statements[statements.high].exprExpression
          val = evaluate(expr)

        echo $val
      else:
        interpret statements
    except ParserException as e:
      echo $e
    finally:
      hadError = false
      hadRuntimeError = false

when isMainModule:
  let argv = commandLineParams()

  if argv.len > 0:
    runFile(argv[0])
  else:
    runPrompt()
