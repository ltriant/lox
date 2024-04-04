import std/
  options,
  strformat,
  tables

import errors
import types

type
  LoxFunction* = ref object
    closure*: Environment
    arity*: uint
    call*: (proc (env: var Environment, args: seq[Object]): Object)

  Environment* = ref object
    values: Table[string, Object]
    functions: Table[string, LoxFunction]
    enclosing: Option[Environment]

proc `$`*(e: Environment): string = $e.values

proc newEnvironment*(enclosing: Option[Environment] = none(Environment)): Environment =
  Environment(
    values: initTable[string, Object](),
    functions: initTable[string, LoxFunction](),
    enclosing: enclosing,
  )

proc ancestor(e: Environment, distance: uint): Environment =
  var env = some(e)

  for _ in 0 ..< distance:
    env = env.flatMap(proc (inner: Environment): Option[Environment] = inner.enclosing)

  return env.get

proc define*(e: var Environment, key: Token, val: Object) =
  e.values[key.lexeme] = val

proc defineFun*(e: var Environment, key: string, val: LoxFunction) =
  e.values[key] = newIdentifier(key)
  e.functions[key] = val

proc assign*(e: var Environment, key: Token, val: Object) =
  if e.values.contains(key.lexeme):
    e.values[key.lexeme] = val
    return

  if e.enclosing.isSome:
    var enclosing = e.enclosing.get
    enclosing.assign key, val
    return

  raise RuntimeError(token: key, msg: fmt"Undefined variable '{key.lexeme}'")

proc assignAt*(e: var Environment, distance: uint, key: Token, val: Object) =
  e.ancestor(distance).values[key.lexeme] = val

proc get*(e: Environment, key: Token): Object =
  if e.values.contains(key.lexeme):
    return e.values[key.lexeme]

  if e.enclosing.isSome:
    let enclosing = e.enclosing.get
    return enclosing.get(key)

  raise RuntimeError(token: key, msg: fmt"Undefined variable '{key.lexeme}'.")

proc getFunc*(e: Environment, key: Object): LoxFunction =
  if not key.isCallable():
    raise RuntimeException(msg: "Can't call something that is not an identifier")

  if e.functions.contains(key.identifierVal):
    return e.functions[key.identifierVal]

  if e.enclosing.isSome:
    let enclosing = e.enclosing.get
    return enclosing.getFunc(key)

  raise RuntimeException(msg: fmt"Undefined function '{key.identifierVal}'.")

proc getAt*(e: Environment, distance: uint, name: string): Object =
  e.ancestor(distance).values[name]
