import std/
  hashes,
  options,
  sequtils,
  strformat,
  strutils,
  sugar

type
  TokenKind* = enum
    # Single-character tokens
    tLeftParen    # (
    tRightParen   # )
    tLeftBrace    # {
    tRightBrace   # }
    tComma        # ,
    tDot          # .
    tMinus        # -
    tPlus         # +
    tSemicolon    # ;
    tSlash        # /
    tStar         # *

    # 1 or 2 character tokens
    tBang         # !
    tBangEqual    # !=
    tEqual        # =
    tEqualEqual   # ==
    tGreater      # >
    tGreaterEqual # >=
    tLess         # <
    tLessEqual    # <=

    # Literals
    tIdentifier   # [a-zA-Z][a-ZA-Z0-9]
    tString       # "string"
    tNumber       # 123.45

    # Keywords
    tAnd          # and
    tClass        # class
    tElse         # else
    tFalse        # false
    tFun          # fun
    tFor          # for
    tIf           # if
    tNil          # nil
    tOr           # or
    tPrint        # print
    tReturn       # return
    tSuper        # super
    tThis         # this
    tTrue         # true
    tVar          # var
    tWhile        # while

    tEOF          # end of file

  Token* = object
    lexeme*: string
    line*: int
    case kind*: TokenKind
    of tNumber:
      floatVal*: float
    else:
      strVal*: string

  ObjectKind* = enum
    oString
    oNumber
    oBoolean
    oNil
    oUndefined
    oIdentifier

  Object* = object
    case kind*: ObjectKind
      of oString:
        strVal*: string
      of oNumber:
        floatVal*: float
      of oBoolean:
        boolVal*: bool
      of oNil:
        nilVal*: int
      of oUndefined:
        undefVal*: int
      of oIdentifier:
        identifierVal*: string

  ExprKind* = enum
    eBinary
    eUnary
    eGrouping
    eLiteral
    eVariable
    eAssign
    eLogical
    eCall

  Expr* = ref object
    case kind*: ExprKind
    of eBinary:
      binaryOp*: Token
      binLeft*, binRight*: Expr
    of eUnary:
      unaryOp*: Token
      unaryRight*: Expr
    of eGrouping:
      expression*: Expr
    of eLiteral:
      literalValue*: Object
    of eVariable:
      varName*: Token
    of eAssign:
      assignToken*: Token
      assignValue*: Expr
    of eLogical:
      logicalLeft*: Expr
      logicalOp*: Token
      logicalRight*: Expr
    of eCall:
      callCallee*: Expr
      callParen*: Token
      callArguments*: seq[Expr]

  StmtKind* = enum
    sExpression
    sPrint
    sVar
    sBlock
    sIf
    sWhile
    sFun
    sReturn

  Stmt* = ref object
    case kind*: StmtKind
    of sExpression:
      exprExpression*: Expr
    of sPrint:
      printExpression*: Expr
    of sVar:
      varToken*: Token
      varExpression*: Expr
    of sBlock:
      blockStmts*: seq[Stmt]
    of sIf:
      ifCondition*: Expr
      ifThenBranch*: Stmt
      ifElseBranch*: Option[Stmt]
    of sWhile:
      whileCondition*: Expr
      whileBody*: Stmt
    of sFun:
      funName*: Token
      funParams*: seq[Token]
      funBody*: seq[Stmt]
    of sReturn:
      retKeyword*: Token
      retValue*: Option[Expr]

proc newBoolean*(v: bool): Object = Object(kind: oBoolean, boolVal: v)
proc newNumber*(v: float): Object = Object(kind: oNumber, floatVal: v)
proc newString*(v: string): Object = Object(kind: oString, strVal: v)
proc newNil*: Object = Object(kind: oNil, nilVal: 0)
proc newUndefined*: Object = Object(kind: oUndefined, undefVal: 0)
proc newIdentifier*(v: string): Object = Object(kind: oIdentifier, identifierVal: v)

proc `$`*(t: Token): string =
  case t.kind
  of tNumber: fmt"{t.kind} {t.lexeme} {t.floatVal}"
  of tString: fmt"{t.kind} {t.lexeme} {t.strVal}"
  else: fmt"{t.kind} {t.lexeme}"

proc `$`*(e: Expr): string =
  case e.kind
  of eBinary:
    fmt"{e.binLeft} {e.binaryOp.lexeme} {e.binRight}"
  of eUnary:
    fmt"{e.unaryOp.lexeme}{e.unaryRight}"
  of eGrouping:
    fmt"({e.expression})"
  of eLiteral:
    $e.literalValue
  of eVariable:
    e.varName.lexeme # TODO
  of eAssign:
    fmt"{e.assignToken.lexeme} = {e.assignValue}"
  of eLogical:
    fmt"{e.logicalLeft} {e.logicalOp.lexeme} {e.logicalRight}"
  of eCall:
    let paramList = e.callArguments.map(a => $a).join(", ")
    fmt"{e.callCallee}({paramList})"

proc `$`*(o: Object): string =
  case o.kind
  of oString:
    o.strVal
  of oNumber:
    let rv = $o.floatVal
    if rv.endsWith(".0"):
      rv[0 .. rv.high - 2]
    else:
      rv
  of oBoolean:
    $o.boolVal
  of oNil:
    "nil"
  of oUndefined:
    "undef"
  of oIdentifier:
    fmt"<fn {o.identifierVal}>"

proc `==`*(a, b: Object): bool =
  if a.kind == oString and b.kind == oString:
    a.strVal == b.strVal
  elif a.kind == oNumber and b.kind == oNumber:
    a.floatVal == b.floatVal
  elif a.kind == oBoolean and b.kind == oBoolean:
    a.boolVal == b.boolVal
  elif a.kind == oNil and b.kind == oNil:
    true
  else:
    false

proc `!=`*(a, b: Object): bool =
  not (a == b)

proc isTruthy*(o: Object): bool =
  case o.kind
  #of oString:
  #  a.strVal.len > 0
  #of oNumber:
  #  a.floatVal > 0.0
  of oBoolean:
    o.boolVal != false
  of oNil:
    false
  else:
    true

func isCallable*(o: Object): bool =
  o.kind == oIdentifier

func hash*(o: Object): Hash =
  case o.kind
  of oString:
    o.strVal.hash
  of oNumber:
    o.floatVal.hash
  of oBoolean:
    o.boolVal.hash
  of oNil:
    o.nilVal.hash
  of oUndefined:
    o.undefVal.hash
  of oIdentifier:
    o.identifierVal.hash

func hash*(t: Token): Hash =
  !$(t.lexeme.hash !& t.line.hash)

func hash*(e: Expr): Hash =
  case e.kind
  of eBinary:
    !$(e.binaryOp.hash !& e.binLeft.hash !& e.binRight.hash)
  of eUnary:
    !$(e.unaryOp.hash !& e.unaryRight.hash)
  of eGrouping:
    e.expression.hash
  of eLiteral:
    e.literalValue.hash
  of eVariable:
    e.varName.hash
  of eAssign:
    !$(e.assignToken.hash !& e.assignValue.hash)
  of eLogical:
    !$(e.logicalLeft.hash !& e.logicalOp.hash !& e.logicalRight.hash)
  of eCall:
    var h = e.callCallee.hash !& e.callParen.hash
    for a in e.callArguments:
      h = h !& a.hash
    h
