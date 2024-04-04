import std/
  options,
  strformat

import errors
import types

type
  Parser = object
    tokens: seq[Token]
    current: int

proc newParser*(tokens: seq[Token]): Parser =
  Parser(
    tokens: tokens,
    current: 0,
  )

func peek(p: Parser): Token =
  p.tokens[p.current]

func previous(p: Parser): Token =
  p.tokens[p.current - 1]

func isAtEnd(p: Parser): bool =
  p.peek().kind == tEOF

proc advance(p: var Parser): Token =
  if not p.isAtEnd():
    p.current += 1

  return p.previous

func check(p: Parser, t: TokenKind): bool =
  not p.isAtEnd() and p.peek().kind == t

proc match(p: var Parser, tokens: varargs[TokenKind]): bool =
  ## Check if the next token matches one of the passed in kinds of token. If it does, the
  ## parser is advanced forward to the next token.
  for t in tokens:
    if p.check(t):
      discard p.advance()
      return true

  return false

proc consume(p: var Parser, tok: TokenKind, message: string): Token =
  ## Assert what the next token is. If the next token does not match, raise an error.
  if p.check(tok):
    return p.advance()

  error p.peek(), message
  raise ParserException(msg: message)

# Because the below functions all call each other, we forward-declare them all
proc primary(p: var Parser): Expr
proc call(p: var Parser): Expr
proc unary(p: var Parser): Expr
proc factor(p: var Parser): Expr
proc term(p: var Parser): Expr
proc comparison(p: var Parser): Expr
proc equality(p: var Parser): Expr
proc logicalAnd(p: var Parser): Expr
proc logicalOr(p: var Parser): Expr
proc assignment(p: var Parser): Expr
proc expression(p: var Parser): Expr

proc printStatement(p: var Parser): Stmt
proc expressionStatement(p: var Parser): Stmt
proc whileStatement(p: var Parser): Stmt
proc forStatement(p: var Parser): Stmt
proc ifStatement(p: var Parser): Stmt
proc stmtBlock(p: var Parser): Stmt
proc statement(p: var Parser): Stmt
proc varDeclaration(p: var Parser): Stmt
proc declaration(p: var Parser): Stmt

proc primary(p: var Parser): Expr =
  if p.match(tFalse):
    return Expr(kind: eLiteral, literalValue: newBoolean(false))
  elif p.match(tTrue):
    return Expr(kind: eLiteral, literalValue: newBoolean(true))
  elif p.match(tNil):
    return Expr(kind: eLiteral, literalValue: newNil())
  elif p.match(tNumber):
    return Expr(kind: eLiteral, literalValue: newNumber(p.previous().floatVal))
  elif p.match(tString):
    return Expr(kind: eLiteral, literalValue: newString(p.previous().strVal))
  elif p.match(tLeftParen):
    let expr = p.expression()
    discard p.consume(tRightParen, "Expect ')' after expression.")
    return Expr(kind: eGrouping, expression: expr)
  elif p.match(tIdentifier):
    return Expr(kind: eVariable, varName: p.previous())

  error p.peek(), "Expected expression."
  raise ParserException(msg: "Expected expression.")

proc finishCall(p: var Parser, callee: Expr): Expr =
  var args = newSeq[Expr]()

  if not p.check(tRightParen):
    args.add p.expression()

    while p.match(tComma):
      args.add p.expression()

      if args.len > 255:
        error p.peek(), "Can't have more than 255 arguments"

  let paren = p.consume(tRightParen, "Expect ')' after arguments.")

  Expr(
    kind: eCall,
    callCallee: callee,
    callParen: paren,
    callArguments: args,
  )

proc call(p: var Parser): Expr =
  var expr = p.primary();

  while true:
    if p.match(tLeftParen):
      expr = p.finishCall(expr)
    else:
      break

  expr

proc unary(p: var Parser): Expr =
  if p.match(tBang, tMinus):
    let
      operator = p.previous()
      right = p.unary()

    return Expr(kind: eUnary, unaryOp: operator, unaryRight: right)

  return p.call()

proc factor(p: var Parser): Expr =
  var expr = p.unary()

  while p.match(tSlash, tStar):
    let
      operator = p.previous()
      right = p.unary()

    expr = Expr(
      kind: eBinary,
      binLeft: expr,
      binaryOp: operator,
      binRight: right
    )

  expr

proc term(p: var Parser): Expr =
  var expr = p.factor()

  while p.match(tMinus, tPlus):
    let
      operator = p.previous()
      right = p.factor()

    expr = Expr(
      kind: eBinary,
      binLeft: expr,
      binaryOp: operator,
      binRight: right
    )

  expr

proc comparison(p: var Parser): Expr =
  var expr = p.term()

  while p.match(tGreater, tGreaterEqual, tLess, tLessEqual):
    let
      operator = p.previous()
      right = p.term()

    expr = Expr(
      kind: eBinary,
      binLeft: expr,
      binaryOp: operator,
      binRight: right
    )
  
  expr

proc equality(p: var Parser): Expr =
  var expr = p.comparison()

  while p.match(tBangEqual, tEqualEqual):
    let
      operator = p.previous()
      right = p.comparison()

    expr = Expr(
        kind: eBinary,
        binLeft: expr,
        binaryOp: operator,
        binRight: right
      )

  expr

proc logicalAnd(p: var Parser): Expr =
  var expr = p.equality()

  while p.match(tAnd):
    let
      operator = p.previous()
      right = p.equality()

    expr = Expr(
      kind: eLogical,
      logicalLeft: expr,
      logicalOp: operator,
      logicalRight: right,
    )

  expr

proc logicalOr(p: var Parser): Expr =
  var expr = p.logicalAnd()

  while p.match(tOr):
    let
      operator = p.previous()
      right = p.logicalAnd()

    expr = Expr(
      kind: eLogical,
      logicalLeft: expr,
      logicalOp: operator,
      logicalRight: right,
    )

  expr

proc assignment(p: var Parser): Expr =
  var expr = p.logicalOr()

  if p.match(tEqual):
    let
      equals = p.previous()
      val = p.assignment()

    if expr.kind == eVariable:
      let name = expr.varName
      return Expr(
        kind: eAssign,
        assignToken: name,
        assignValue: val
      )

    error equals, "Invalid assigment target."

  expr

proc expression(p: var Parser): Expr =
  p.assignment()

proc ifStatement(p: var Parser): Stmt =
  discard p.consume(tLeftParen, "Expect '(' after 'if'.")
  let condition = p.expression()
  discard p.consume(tRightParen, "Expect ')' after condition.")

  Stmt(
    kind: sIf,
    ifCondition: condition,
    ifThenBranch: p.statement(),
    ifElseBranch:
      if p.match(tElse):
        some(p.statement())
      else:
        none(Stmt)
  )

proc printStatement(p: var Parser): Stmt =
  let value = p.expression()
  discard p.consume(tSemicolon, "Expect ';' after value.")
  Stmt(kind: sPrint, printExpression: value)

proc whileStatement(p: var Parser): Stmt =
  discard p.consume(tLeftParen, "Expect '(' after 'while'.")
  let cond = p.expression();
  discard p.consume(tRightParen, "Expect ')' after while condition.")
  let body = p.statement();
  Stmt(kind: sWhile, whileCondition: cond, whileBody: body)

proc forStatement(p: var Parser): Stmt =
  # Turns for loops into while loops
  discard p.consume(tLeftParen, "Expect '(' after 'for'.")

  # TODO port to Option[Stmt]
  var initializer: Stmt = nil
  if p.match(tSemicolon):
    discard
  elif p.match(tVar):
    initializer = p.varDeclaration()
  else:
    initializer = p.expressionStatement()

  var condition: Expr = nil
  if not p.check(tSemicolon):
    condition = p.expression()
  discard p.consume(tSemicolon, "Expect ';' after loop condition.")

  var increment: Expr = nil
  if not p.check(tRightParen):
    increment = p.expression()
  discard p.consume(tRightParen, "Expect ')' after loop clauses.")

  var body = p.statement()

  if increment != nil:
    body = Stmt(
      kind: sBlock,
      blockStmts: @[
        body,
        Stmt(
          kind: sExpression,
          exprExpression: increment,
        )
      ]
    )

  if condition == nil:
    condition = Expr(
      kind: eLiteral,
      literalValue: newBoolean(true)
    )

  body = Stmt(
    kind: sWhile,
    whileCondition: condition,
    whileBody: body,
  )

  if initializer != nil:
    body = Stmt(
      kind: sBlock,
      blockStmts: @[
        initializer,
        body,
      ],
    )

  body

proc function(p: var Parser, kind: string): Stmt =
  let name = p.consume(tIdentifier, fmt"Expect {kind} name.")
  discard p.consume(tLeftParen, fmt"Expect '(' after {kind} name.")

  var params = newSeq[Token]()
  if not p.check(tRightParen):
    params.add(p.consume(tIdentifier, "Expect parameter name."))
    while p.match(tComma):
      params.add(p.consume(tIdentifier, "Expect parameter name."))
      if params.len >= 255:
        error p.peek(), "Can't have more than 255 parameters."

  discard p.consume(tRightParen, "Expect ')' after parameters.")
  discard p.consume(tLeftBrace, fmt"Expect '{{' before {kind} body.")
  let body = p.stmtBlock()

  Stmt(kind: sFun, funName: name, funParams: params, funBody: body.blockStmts)

proc expressionStatement(p: var Parser): Stmt =
  let value = p.expression()
  discard p.consume(tSemicolon, "Expect ';' after expression.")
  Stmt(kind: sExpression, exprExpression: value)

proc stmtBlock(p: var Parser): Stmt =
  var stmts = newSeq[Stmt]()

  while not p.check(tRightBrace) and not p.isAtEnd():
    stmts.add p.declaration()

  discard p.consume(tRightBrace, "Expect '}' after block.")

  Stmt(kind: sBlock, blockStmts: stmts)

proc returnStatement(p: var Parser): Stmt =
  let keyword = p.previous()
  var value = none(Expr)

  if not p.check(tSemicolon):
    value = some(p.expression())

  discard p.consume(tSemicolon, "Expect ';' after return value.")

  return Stmt(kind: sReturn, retKeyword: keyword, retValue: value)

proc statement(p: var Parser): Stmt =
  if p.match(tFor):
    return p.forStatement()
  elif p.match(tIf):
    return p.ifStatement()
  elif p.match(tWhile):
    return p.whileStatement()
  elif p.match(tReturn):
    return p.returnStatement()
  elif p.match(tPrint):
    return p.printStatement()
  elif p.match(tLeftBrace):
    return p.stmtBlock()
  elif p.match(tFun):
    return p.function("function")

  return p.expressionStatement()

proc varDeclaration(p: var Parser): Stmt =
  let name = p.consume(tIdentifier, "Expect variable name.")

  var expr = none(Expr)
  if p.match(tEqual):
    expr = some(p.expression())

  discard p.consume(tSemicolon, "Expect ';' after variable declaration.")
  Stmt(kind: sVar, varToken: name, varExpression: expr)

proc declaration(p: var Parser): Stmt =
  if p.match(tVar):
    return p.varDeclaration()

  return p.statement()

proc parse*(p: var Parser): seq[Stmt] =
  while not p.isAtEnd():
    result.add p.declaration()

