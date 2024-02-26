import std/strformat

import types

var hadError* = false
var hadRuntimeError* = false

type
  RuntimeException* = ref object of Defect

  RuntimeError* = ref object of Defect
    token*: Token

  ParserException* = ref object of Defect

proc report(line: int, where: string, message: string) =
  ## Outputs a message, and sets an error state.
  echo fmt"[line {line}] Error {where}: {message}"

  hadError = true

proc error*(line: int, message: string) =
  ## Logs an error messsage.
  report line, "", message

proc error*(token: Token, message: string) =
  ## Logs an error message for a specific Token
  if token.kind == tEOF:
    report token.line, "at end", message
  else:
    report token.line, fmt"at {token.lexeme}", message

proc `$`*(e: ParserException): string =
  fmt"ParserException: {e.msg}"
