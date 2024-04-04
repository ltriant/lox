import std/
  deques,
  options,
  tables

import errors
import interpreter
import types

type
  Scope = TableRef[string, bool]

  FunctionType = enum
    fNone
    fFunction

  Resolver = ref object
    scopes: Deque[Scope]
    currentFunction: FunctionType

proc newResolver*: Resolver =
  Resolver(
    scopes: initDeque[Scope](),
    currentFunction: fNone,
  )

proc beginScope(r: var Resolver) =
  r.scopes.addLast newTable[string, bool]()

proc endScope(r: var Resolver) =
  let scope = r.scopes.popLast

proc declare(r: var Resolver, name: Token) =
  if r.scopes.len == 0:
    return

  var scope = r.scopes.peekLast

  if scope.contains(name.lexeme):
    error name, "Already a variable with this name in this scope."

  scope[name.lexeme] = false

proc define(r: var Resolver, name: Token) =
  if r.scopes.len == 0:
    return

  var scope = r.scopes.peekLast
  scope[name.lexeme] = true

# Forward declaration
proc resolve*(r: var Resolver, statements: seq[Stmt])

proc resolveLocal(r: var Resolver, expr: Expr, name: Token) =
  for i in countdown(r.scopes.len - 1, 0):
    if r.scopes[i].contains(name.lexeme):
      interpreter.resolve(expr, uint(r.scopes.len() - 1 - i))
      return

proc resolveExpr(r: var Resolver, expr: Expr) =
  case expr.kind
  of eVariable:
    if r.scopes.len > 0:
      let scope = r.scopes.peekLast
      if scope.contains(expr.varName.lexeme) and scope[expr.varName.lexeme] == false:
        error expr.varName, "Can't read local variable in its own initializer."

    r.resolveLocal expr, expr.varName
  of eAssign:
    r.resolveExpr expr.assignValue
    r.resolveLocal expr, expr.assignToken
  of eBinary:
    r.resolveExpr expr.binLeft
    r.resolveExpr expr.binRight
  of eUnary:
    r.resolveExpr expr.unaryRight
  of eGrouping:
    r.resolveExpr expr.expression
  of eLogical:
    r.resolveExpr expr.logicalLeft
    r.resolveExpr expr.logicalRight
  of eCall:
    r.resolveExpr expr.callCallee

    for arg in expr.callArguments:
      r.resolveExpr arg
  of eLiteral:
    discard

proc resolveFunction(r: var Resolver, fun: Stmt, funcType: FunctionType) =
  let enclosingFunction = r.currentFunction
  r.currentFunction = funcType

  r.beginScope
  for param in fun.funParams:
    r.declare param
    r.define param
  r.resolve fun.funBody
  r.endScope

  r.currentFunction = enclosingFunction

proc resolveStmt(r: var Resolver, stmt: Stmt) =
  case stmt.kind
  of sBlock:
    r.beginScope
    r.resolve stmt.blockStmts
    r.endScope
  of sVar:
    r.declare stmt.varToken
    r.resolveExpr stmt.varExpression
    r.define stmt.varToken
  of sFun:
    r.declare stmt.funName
    r.define stmt.funName
    r.resolveFunction stmt, fFunction
  of sExpression:
    r.resolveExpr stmt.exprExpression
  of sIf:
    r.resolveExpr stmt.ifCondition
    r.resolveStmt stmt.ifThenBranch
    if stmt.ifElseBranch.isSome:
      r.resolveStmt stmt.ifElseBranch.get
  of sPrint:
    r.resolveExpr stmt.printExpression
  of sReturn:
    if r.currentFunction == fNone:
      error stmt.retKeyword, "Can't return from top-level code."

    if stmt.retValue.isSome:
      r.resolveExpr stmt.retValue.get
  of sWhile:
    r.resolveExpr stmt.whileCondition
    r.resolveStmt stmt.whileBody

proc resolve*(r: var Resolver, statements: seq[Stmt]) =
  for s in statements:
    r.resolveStmt s
