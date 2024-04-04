import std/
  options,
  sequtils,
  strformat,
  sugar,
  tables,
  times

import environment
import errors
import types

type
  ReturnValue = ref object of CatchableError
    value: Option[Object]

var
  globals = newEnvironment()
  env = globals # newEnvironment(some(globals))
  locals = newTable[Expr, uint]()

globals.defineFun "clock", LoxFunction(
  closure: env,
  arity: 0,
  call: proc (env: var Environment, args: seq[Object]): Object = newNumber(epochTime())
)

proc checkNumberOperands(t: Token, os: varargs[Object]) =
  for o in os:
    if o.kind != oNumber:
      raise RuntimeError(token: t, msg: "Operands must be numbers.")

proc lookUpVariable(name: Token, expr: Expr): Object =
  if locals.contains(expr):
    env.getAt(locals[expr], name.lexeme)
  else:
    globals.get(name)

proc evaluate*(e: Expr): Object =
  case e.kind
  of eLiteral:
    return e.literalValue
  of eGrouping:
    return evaluate(e.expression)
  of eUnary:
    let right = evaluate(e.unaryRight)

    case e.unaryOp.kind:
      of tMinus:
        checkNumberOperands e.unaryOp, right
        return Object(kind: oNumber, floatVal: -1.0 * right.floatVal)
      of tBang:
        return Object(kind: oBoolean, boolVal: not right.isTruthy())
      else:
        return Object(kind: oNil, nilVal: 0)
  of eBinary:
    let
      left = evaluate(e.binLeft)
      right = evaluate(e.binRight)

    case e.binaryOp.kind
    of tMinus:
      checkNumberOperands e.binaryOp, left, right
      return Object(kind: oNumber, floatVal: left.floatVal - right.floatVal)
    of tPlus:
      if left.kind == oNumber and right.kind == oNumber:
        return Object(kind: oNumber, floatVal: left.floatVal + right.floatVal)
      elif left.kind == oString and right.kind == oString:
        return Object(kind: oString, strVal: left.strVal & right.strVal)
      elif left.kind == oNumber and right.kind == oString:
        return Object(kind: oString, strVal: $left & right.strVal)
      elif left.kind == oString and right.kind == oNumber:
        return Object(kind: oString, strVal: left.strVal & $right)
      else:
        raise RuntimeError(token: e.binaryOp, msg: "Operands must be two numbers or two strings.")
    of tSlash:
      checkNumberOperands e.binaryOp, left, right
      return Object(kind: oNumber, floatVal: left.floatVal / right.floatVal)
    of tStar:
      checkNumberOperands e.binaryOp, left, right
      return Object(kind: oNumber, floatVal: left.floatVal * right.floatVal)
    of tGreater:
      checkNumberOperands e.binaryOp, left, right
      return Object(kind: oBoolean, boolVal: left.floatVal > right.floatVal)
    of tGreaterEqual:
      checkNumberOperands e.binaryOp, left, right
      return Object(kind: oBoolean, boolVal: left.floatVal >= right.floatVal)
    of tLess:
      checkNumberOperands e.binaryOp, left, right
      return Object(kind: oBoolean, boolVal: left.floatVal < right.floatVal)
    of tLessEqual:
      checkNumberOperands e.binaryOp, left, right
      return Object(kind: oBoolean, boolVal: left.floatVal <= right.floatVal)
    of tEqualEqual:
      return Object(kind: oBoolean, boolVal: left == right)
    of tBangEqual:
      return Object(kind: oBoolean, boolVal: left != right)
    else:
      return Object(kind: oNil, nilVal: 0)
  of eVariable:
    let val = lookUpVariable(e.varName, e)
    if val.kind == oUndefined:
      raise RuntimeError(token: e.varName, msg: "Variable is not defined.")
    return val
  of eAssign:
    let val = evaluate(e.assignValue)

    if locals.contains(e):
      env.assignAt locals[e], e.assignToken, val
    else:
      globals.assign e.assignToken, val

    return val
  of eLogical:
    let left = evaluate(e.logicalLeft)

    case e.logicalOp.kind
    of tOr:
      if left.isTruthy():
        return left
    of tAnd:
      if not left.isTruthy():
        return left
    else:
      discard

    return evaluate(e.logicalRight)
  of eCall:
    let callee = evaluate(e.callCallee)

    if not callee.isCallable():
      raise RuntimeError(
        token: e.callParen,
        msg: "Can only call functions and classes."
      )

    let args = e.callArguments.map(a => evaluate(a))

    var
      fun = env.getFunc(callee)
      env = newEnvironment(some(fun.closure))

    if args.len.uint != fun.arity:
        raise RuntimeError(
          token: e.callParen,
          msg: fmt"Expected {fun.arity} arguments but got {args.len}."
        )

    return fun.call(env, args)

proc runtimeError(e: RuntimeError) =
  echo "{e.msg}\n[line {e.token.line}]".fmt
  hadRuntimeError = true

# Forward-declaring this, since both functions can call each other
proc execute(s: Stmt)
proc executeBlock(stmts: seq[Stmt], newEnv: Environment) =
  let previousEnv = env

  try:
    env = newEnv

    for s in stmts:
      execute s
  finally:
    env = previousEnv

proc execute(s: Stmt) =
  case s.kind
  of sPrint:
    let rv = evaluate(s.printExpression)
    echo $rv
  of sExpression:
    discard evaluate(s.exprExpression)
  of sVar:
    env.define s.varToken, evaluate(s.varExpression)
  of sBlock:
    executeBlock s.blockStmts, newEnvironment(some(env))
  of sIf:
    if evaluate(s.ifCondition).isTruthy():
      execute s.ifThenBranch
    elif s.ifElseBranch.isSome():
      execute s.ifElseBranch.get()
  of sWhile:
    while evaluate(s.whileCondition).isTruthy():
      execute s.whileBody
  of sFun:
    globals.defineFun s.funName.lexeme, LoxFunction(
      closure: env,
      arity: s.funParams.len.uint,
      call: proc (env: var Environment, args: seq[Object]): Object =
        for i, param in s.funParams:
          env.define param, args[i]

        try:
          executeBlock s.funBody, env
        except ReturnValue as e:
          return e.value.get(newNil())

        return newNil()
    )
  of sReturn:
    raise ReturnValue(value: s.retValue.map(v => evaluate(v)))

proc interpret*(statements: seq[Stmt]) =
  try:
    for s in statements:
      execute s
  except RuntimeError as ex:
    runtimeError ex

proc resolve*(expr: Expr, distance: uint) =
  locals[expr] = distance
