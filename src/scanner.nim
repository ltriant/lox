import std/
  sequtils,
  strformat,
  strutils,
  tables

import errors
import prelude
import types

type
  Scanner = object
    source: seq[char]
    tokens: seq[Token]

    start: int
    current: int
    line: int

let keywords = {
  "and": tAnd,
  "class": tClass,
  "else": tElse,
  "false": tFalse,
  "for": tFor,
  "fun": tFun,
  "if": tIf,
  "nil": tNil,
  "or": tOr,
  "print": tPrint,
  "return": tReturn,
  "super": tSuper,
  "this": tThis,
  "true": tTrue,
  "var": tVar,
  "while": tWhile,
}.toTable

proc newScanner*(code: string): Scanner =
  Scanner(
    source: code.toSeq,
    tokens: newSeq[Token](),

    start: 0,
    current: 0,
    line: 1,
  )

func isAtEnd(s: Scanner): bool =
  s.current >= s.source.len

func peek(s: Scanner): char =
  ## Returns the current char we're scanning
  if s.isAtEnd:
    return '\0'

  return s.source[s.current]

func peekNext(s: Scanner): char=
  ## Returns the char after the current one we're scanning
  if s.current + 1 >= s.source.len:
    return '\0'

  return s.source[s.current + 1]

proc advance(s: var Scanner): char =
  ## Returns the current char we're scanning, and advances the state of the scanner to the next character.
  result = s.source[s.current]
  s.current += 1

proc match(s: var Scanner, expected: char): bool =
  ## Tests if the current character matches an expected character.
  if s.isAtEnd:
    return false

  if s.source[s.current] != expected:
    return false

  s.current += 1
  return true

proc addToken(s: var Scanner, tokenType: TokenKind) =
  ## Adds a scanned token to the scanner result.
  let txt = s.source[s.start ..< s.current].toString

  s.tokens.add Token(
    kind: tokenType,
    lexeme: txt,
    line: s.line,
  )

proc addStringToken(s: var Scanner, val: string) =
  ## Adds a string tokenn to the scanner result.
  let txt = s.source[s.start ..< s.current].toString

  s.tokens.add Token(
    kind: tString,
    lexeme: txt,
    line: s.line,
    strVal: val,
  )

proc addFloatToken(s: var Scanner, val: float) =
  ## Adds a number/float token to the scanner result.
  let txt = s.source[s.start ..< s.current].toString

  s.tokens.add Token(
    kind: tNumber,
    lexeme: txt,
    line: s.line,
    floatVal: val,
  )

proc scanNumber(s: var Scanner) =
  ## Scans a floating point number, and adds the token to the scanner result.
  while s.peek.isDigit:
    discard s.advance

  # Look for a fractional part
  if s.peek == '.' and s.peekNext.isDigit:
    # Consume the .
    discard s.advance

    while s.peek.isDigit:
      discard s.advance

  s.addFloatToken s.source[s.start ..< s.current].toString.parseFloat

proc scanString(s: var Scanner) =
  ## Scans a string, and adds the token to the scanner result.
  while s.peek != '"' and not s.isAtEnd:
    if s.peek == '\n':
      s.line += 1

    discard s.advance

  if s.isAtEnd:
    error s.line, "Unterminated string."
    return

  # The closing "
  discard s.advance

  let value = s.source[s.start + 1 ..< s.current - 1].toString
  s.addStringToken value

proc scanIdentifier(s: var Scanner) =
  ## Scans an identifier, and adds the token to the scanner result.
  while s.peek.isAlphaNumeric:
    discard s.advance

  let txt = s.source[s.start ..< s.current].toString

  if keywords.hasKey(txt):
    s.addToken keywords[txt]
  else:
    s.addToken tIdentifier

proc scanToken(s: var Scanner) =
  ## Scans a single token.
  let c = s.advance

  case c
  of '(': s.addToken tLeftParen
  of ')': s.addToken tRightParen
  of '{': s.addToken tLeftBrace
  of '}': s.addToken tRightBrace
  of ',': s.addToken tComma
  of '.': s.addToken tDot
  of '-': s.addToken tMinus
  of '+': s.addToken tPlus
  of ';': s.addToken tSemicolon
  of '*': s.addToken tStar
  of '!': s.addToken if s.match('='): tBangEqual else: tBang
  of '=': s.addToken if s.match('='): tEqualEqual else: tEqual
  of '<': s.addToken if s.match('='): tLessEqual else: tLess
  of '>': s.addToken if s.match('='): tGreaterEqual else: tGreater
  of '/':
    if s.match('/'):
      # Ignore everything after a comment
      while s.peek != '\n' and not s.isAtEnd:
        discard s.advance
    else:
      s.addToken tSlash
  of ' ', '\r', '\t': discard
  of '\n': s.line += 1
  of '"': s.scanString
  else:
    if c.isDigit:
      s.scanNumber
    elif c.isAlphaAscii:
      s.scanIdentifier
    else:
      error s.line, fmt"Unexpected character {c}."

proc scanTokens*(s: var Scanner): seq[Token] =
  ## Scan all of the tokens until we reach the end of the source.
  while not s.isAtEnd:
    s.start = s.current
    s.scanToken

  s.tokens.add Token(
    kind: tEOF,
    lexeme: "",
    line: s.line,
  )

  s.tokens

