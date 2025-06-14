package cavia
package codegen

import java.util.IdentityHashMap
import scala.jdk.CollectionConverters._
import scala.collection.mutable
import mutable.ArrayBuffer

import core.*
import ast.{expr, Wasm}, expr.Expr
import Expr.{Term, Pattern}
import Wasm.*
import typechecking.*
import Expr.{PrimitiveOp, Definition, ArrayPrimitiveOp}
import reporting.trace
import core.ast.expr.Expr

object CodeGenerator:
  case class ClosureTypeInfo(funcTypeSym: Symbol, closTypeSym: Symbol)

  enum BinderInfo:
    /** A binder with a symbol in the target language created for it. */
    case Sym(binder: Expr.Binder, sym: Symbol)
    /** A binder that is created for a closure. */
    case ClosureSym(binder: Expr.Binder, sym: Symbol, funSymbol: Symbol)
    /** A binder that is bound to a type-polymorphic function. 
     * `generator` is a function that takes a list of specialised type binders and produces the closure.
     * `specMap` maps specialisation signatures to the pair of (worker symbol, function symbol).
    */
    case PolyClosureSym(binder: Expr.Binder.TermBinder, params: List[Expr.Binder], generator: List[BinderInfo] => (List[Instruction], Symbol), specMap: mutable.Map[SpecSig, (Symbol, Symbol)])
    /** This binder is inaccessible. It may be a variable not accessed by a closure. Or it is a capture binder. */
    case Inaccessible(binder: Expr.Binder)
    /** This is a binder without a runtime representation, i.e. an erased capability. For instance, the arena handle. */
    case Erased(binder: Expr.Binder)
    /** This is a type binder specialized to a concrete value type in the target. */
    case Specialized(binder: Expr.Binder, tpe: ValType)
    /** This is a type binder that is abstract. */
    case Abstract(binder: Expr.Binder)

    /** Retrieve the source binder. */
    val binder: Expr.Binder

  /** Target langauge denotation of top level `val` or `def` definitions. */
  enum DefInfo:
    /** A non-polymorphic function in the source is translated directly to a function in the target. */
    case FuncDef(funcSym: Symbol, workerSym: Symbol)
    /** A polymorphic function in the source is translated to a family function in the target, each being specialised to a concrete signature. */
    case PolyFuncDef(
      typeParams: List[Expr.Binder],
      body: Expr.Term,
      specMap: mutable.Map[SpecSig, FuncDef],
    )
    case LazyFuncDef(body: Expr.Closure)
    case GlobalDef(globalSym: Symbol)

  case class StructInfo(sym: Symbol, nameMap: Map[String, Symbol], layoutInfo: LayoutInfo, tagNumber: Option[Int] = None)

  case class ExtensionInfo(methodMap: Map[String, MethodInfo])
  enum MethodInfo:
    case SimpleMethod(sym: Symbol, tpe: FuncType)
    case PolyMethod(
      baseName: String, 
      computeType: (List[Expr.Type | Expr.CaptureSet]) => FuncType,
      generator: (List[Expr.Type | Expr.CaptureSet], Symbol) => Unit, 
      specMap: mutable.Map[SpecSig, SimpleMethod]
    )

  /** Signature of a specialisation of a struct or a method. */
  case class SpecSig(targs: List[ValType]):
    def encodedName: String =
      nameEncode(targs.map(_.show).mkString("_"))

  enum MemRepr:
    case Plain(tpe: ValType)
    case Ref(refType: ReferenceType)

    def reprType: ValType = this match
      case MemRepr.Plain(tpe) => tpe
      case MemRepr.Ref(refType) => 
        // Reference types are always represented as i32,
        // which is the index in the shadow stack.
        ValType.I32

  case class FieldLayout(offset: Int, repr: MemRepr)

  /** Information of a memory layout. */
  case class LayoutInfo(
    totalSize: Int,
    fields: Map[Symbol, FieldLayout],
  )
  object LayoutInfo:
    def empty: LayoutInfo = LayoutInfo(0, Map.empty)

  case class Context(
    funcs: ArrayBuffer[Func] = ArrayBuffer.empty,
    globals: ArrayBuffer[Global] = ArrayBuffer.empty,
    memories: ArrayBuffer[Memory] = ArrayBuffer.empty,
    tables: ArrayBuffer[Table] = ArrayBuffer.empty,
    exports: ArrayBuffer[Export] = ArrayBuffer.empty,
    imports: ArrayBuffer[ImportFunc] = ArrayBuffer.empty,
    locals: ArrayBuffer[(Symbol, ValType)] = ArrayBuffer.empty,
    types: ArrayBuffer[TypeDef] = ArrayBuffer.empty,
    closureTypes: mutable.Map[FuncType, ClosureTypeInfo] = mutable.Map.empty,
    arrayTypes: mutable.Map[ArrayType, Symbol] = mutable.Map.empty,
    declares: ArrayBuffer[ElemDeclare] = ArrayBuffer.empty,
    binderInfos: List[BinderInfo] = Nil,
    defInfos: mutable.Map[Expr.DefSymbol, DefInfo] = new IdentityHashMap[Expr.DefSymbol, DefInfo]().asScala,
    typeInfos: mutable.Map[Expr.StructSymbol, mutable.Map[SpecSig, StructInfo]] = 
      new IdentityHashMap[Expr.StructSymbol, mutable.Map[SpecSig, StructInfo]]().asScala,
    extensionInfos: mutable.Map[Expr.ExtensionSymbol, mutable.Map[SpecSig, ExtensionInfo]] =
      new IdentityHashMap[Expr.ExtensionSymbol, mutable.Map[SpecSig, ExtensionInfo]]().asScala,
    var startFunc: Option[Symbol] = None,
  ):
    def withLocalSym(binder: Expr.Binder, sym: Symbol): Context =
      copy(binderInfos = BinderInfo.Sym(binder, sym) :: binderInfos)

    def withClosureSym(binder: Expr.Binder, sym: Symbol, funSym: Symbol): Context =
      copy(binderInfos = BinderInfo.ClosureSym(binder, sym, funSym) :: binderInfos)

    def withPolyClosureSym(binder: Expr.Binder.TermBinder, params: List[Expr.Binder], generator: List[BinderInfo] => (List[Instruction], Symbol)): Context =
      copy(binderInfos = BinderInfo.PolyClosureSym(binder, params, generator, mutable.Map.empty) :: binderInfos)

    def withErasedBinder(binder: Expr.Binder): Context =
      copy(binderInfos = BinderInfo.Erased(binder) :: binderInfos)
    
    def usingBinderInfos(binderInfos: List[BinderInfo]): Context =
      copy(binderInfos = binderInfos)

    def withMoreBinderInfos(binderInfos: List[BinderInfo]): Context =
      copy(binderInfos = binderInfos.reverse ++ this.binderInfos)

    def typecheckerCtx: TypeChecker.Context =
      TypeChecker.Context(
        binders = binderInfos.map(_.binder),
        symbols = Nil,
        inferenceState = Inference.InferenceState.empty,
      )

  def ctx(using ctx: Context): Context = ctx

  def emitFunc(f: Func)(using Context): Unit =
    ctx.funcs += f

  def emitExport(e: Export)(using Context): Unit =
    ctx.exports += e

  def emitMemory(m: Memory)(using Context): Unit =
    ctx.memories += m

  def emitGlobal(g: Global)(using Context): Unit =
    ctx.globals += g

  def emitImportFunc(i: ImportFunc)(using Context): Unit =
    ctx.imports += i

  def emitLocal(sym: Symbol, tpe: ValType)(using Context): Unit =
    ctx.locals += (sym -> tpe)

  def emitLocals(locals: List[(Symbol, ValType)])(using Context): Unit =
    locals.foreach: (sym, tpe) =>
      emitLocal(sym, tpe)

  def emitType(t: TypeDef)(using Context): Unit =
    ctx.types += t

  def emitTable(t: Table)(using Context): Unit =
    ctx.tables += t

  def emitElemDeclare(kind: ExportKind, sym: Symbol)(using Context): Unit =
    ctx.declares += ElemDeclare(kind, sym)

  def finalizeLocals(using Context): List[(Symbol, ValType)] =
    val result = ctx.locals.toList
    ctx.locals.clear()
    result

  def genMemoryOp(op: Expr.ArrayPrimitiveOp, args: List[Expr.Term])(using Context): List[Instruction] =
    op match
      case Expr.ArrayGet() => 
        val memSym :: idx :: Nil = args: @unchecked
        val idxInstrs = genTerm(idx)
        val loadInstrs = List(Instruction.Load(ValType.I32, Symbol.Memory))
        idxInstrs ++ loadInstrs
      case Expr.ArraySet() => 
        val memSym :: idx :: value :: Nil = args: @unchecked
        val idxInstrs = genTerm(idx)
        val valueInstrs = genTerm(value)
        val storeInstrs = List(Instruction.Store(ValType.I32, Symbol.Memory))
        val unitInstrs = List(Instruction.I32Const(0))
        idxInstrs ++ valueInstrs ++ storeInstrs ++ unitInstrs
      case Expr.ArrayLen() => 
        val sizeInstrs = List(Instruction.MemorySize(Symbol.Memory))
        sizeInstrs
      case _ => assert(false, "Unsupported memory operation")

  def genArrayPrimOp(op: Expr.ArrayPrimitiveOp, tpe: Expr.Type, targs: List[Expr.Type], args: List[Expr.Term])(using Context): List[Instruction] =
    op match
      case Expr.ArrayNew() => 
        val elemType :: Nil = targs: @unchecked
        val size :: init :: Nil = args: @unchecked
        val elemValType = translateType(elemType)
        val arrType = computeArrayType(tpe)
        val arrSym = createArrayType(arrType)
        val initInstrs = genTerm(init)
        val sizeInstrs = genTerm(size)
        val newInstrs = List(Instruction.ArrayNew(arrSym))
        initInstrs ++ sizeInstrs ++ newInstrs
      case Expr.ArrayGet() => 
        val arr :: idx :: Nil = args: @unchecked
        val arrType = computeArrayType(arr.tpe)
        val arrSym = createArrayType(arrType)
        val arrInstrs = genTerm(arr)
        val idxInstrs = genTerm(idx)
        val getInstrs = List(Instruction.ArrayGet(arrSym))
        arrInstrs ++ idxInstrs ++ getInstrs
      case Expr.ArraySet() => 
        val arr :: idx :: value :: Nil = args: @unchecked
        val arrType = computeArrayType(arr.tpe)
        val arrSym = createArrayType(arrType)
        val arrInstrs = genTerm(arr)
        val idxInstrs = genTerm(idx)
        val valueInstrs = genTerm(value)
        val setInstrs = List(Instruction.ArraySet(arrSym))
        val unitInstrs = List(Instruction.I32Const(0))
        arrInstrs ++ idxInstrs ++ valueInstrs ++ setInstrs ++ unitInstrs
      case Expr.ArrayLen() =>
        val arr :: Nil = args: @unchecked
        val arrInstrs = genTerm(arr)
        val lenInstrs = List(Instruction.ArrayLen)
        arrInstrs ++ lenInstrs

  def resolveBinaryPrimOp(opKind: Expr.BasicPrimOpKind, operandType: ValType): Option[Instruction] =
    (opKind, operandType) match
      case (Expr.BasicPrimOpKind.Add, ValType.I32) => Some(Instruction.I32Add)
      case (Expr.BasicPrimOpKind.Add, ValType.I64) => Some(Instruction.I64Add)
      case (Expr.BasicPrimOpKind.Add, ValType.F64) => Some(Instruction.F64Add)
      case (Expr.BasicPrimOpKind.Mul, ValType.I32) => Some(Instruction.I32Mul)
      case (Expr.BasicPrimOpKind.Mul, ValType.I64) => Some(Instruction.I64Mul)
      case (Expr.BasicPrimOpKind.Mul, ValType.F64) => Some(Instruction.F64Mul)
      case (Expr.BasicPrimOpKind.Sub, ValType.I32) => Some(Instruction.I32Sub)
      case (Expr.BasicPrimOpKind.Sub, ValType.I64) => Some(Instruction.I64Sub)
      case (Expr.BasicPrimOpKind.Sub, ValType.F64) => Some(Instruction.F64Sub)
      case (Expr.BasicPrimOpKind.Div, ValType.I32) => Some(Instruction.I32Div)
      case (Expr.BasicPrimOpKind.Div, ValType.I64) => Some(Instruction.I64Div)
      case (Expr.BasicPrimOpKind.Div, ValType.F64) => Some(Instruction.F64Div)
      case (Expr.BasicPrimOpKind.Rem, ValType.I32) => Some(Instruction.I32Rem)
      case (Expr.BasicPrimOpKind.Rem, ValType.I64) => Some(Instruction.I64Rem)
      case _ => None

  def resolveComparePrimOp(opKind: Expr.BasicPrimOpKind, operandType: ValType): Option[Instruction] =
    (opKind, operandType) match
      case (Expr.BasicPrimOpKind.Eq, ValType.I32) => Some(Instruction.I32Eq)
      case (Expr.BasicPrimOpKind.Eq, ValType.I64) => Some(Instruction.I64Eq)
      case (Expr.BasicPrimOpKind.Eq, ValType.F64) => Some(Instruction.F64Eq)
      case (Expr.BasicPrimOpKind.Neq, ValType.I32) => Some(Instruction.I32Ne)
      case (Expr.BasicPrimOpKind.Neq, ValType.I64) => Some(Instruction.I64Ne)
      case (Expr.BasicPrimOpKind.Neq, ValType.F64) => Some(Instruction.F64Ne)
      case (Expr.BasicPrimOpKind.Lt, ValType.I32) => Some(Instruction.I32Lt)
      case (Expr.BasicPrimOpKind.Lt, ValType.I64) => Some(Instruction.I64Lt)
      case (Expr.BasicPrimOpKind.Lt, ValType.F64) => Some(Instruction.F64Lt)
      case (Expr.BasicPrimOpKind.Lte, ValType.I32) => Some(Instruction.I32Lte)
      case (Expr.BasicPrimOpKind.Lte, ValType.I64) => Some(Instruction.I64Lte)
      case (Expr.BasicPrimOpKind.Lte, ValType.F64) => Some(Instruction.F64Lte)
      case (Expr.BasicPrimOpKind.Gt, ValType.I32) => Some(Instruction.I32Gt)
      case (Expr.BasicPrimOpKind.Gt, ValType.I64) => Some(Instruction.I64Gt)
      case (Expr.BasicPrimOpKind.Gt, ValType.F64) => Some(Instruction.F64Gt)
      case (Expr.BasicPrimOpKind.Gte, ValType.I32) => Some(Instruction.I32Gte)
      case (Expr.BasicPrimOpKind.Gte, ValType.I64) => Some(Instruction.I64Gte)
      case (Expr.BasicPrimOpKind.Gte, ValType.F64) => Some(Instruction.F64Gte)
      case _ => None

  def genSimplePrimOp(args: List[Expr.Term], op: PrimitiveOp)(using Context): List[Instruction] = 
    def argInstrs = args.flatMap(genTerm)
    op match
      // binary ops
      case Expr.BinaryPrimOp(opKind, operandType) if resolveBinaryPrimOp(opKind, translateBaseType(operandType)).isDefined =>
        val instr = resolveBinaryPrimOp(opKind, translateBaseType(operandType)).get
        argInstrs ++ List(instr)
      // negation
      case Expr.UnaryPrimOp(Expr.BasicPrimOpKind.Neg, Expr.BaseType.I32) => List(Instruction.I32Const(0)) ++ argInstrs ++ List(Instruction.I32Sub)
      case Expr.UnaryPrimOp(Expr.BasicPrimOpKind.Neg, Expr.BaseType.I64) => List(Instruction.I64Const(0)) ++ argInstrs ++ List(Instruction.I64Sub)
      case Expr.UnaryPrimOp(Expr.BasicPrimOpKind.Neg, Expr.BaseType.F64) => List(Instruction.F64Const(0.0)) ++ argInstrs ++ List(Instruction.F64Sub)
      // comparison ops
      case Expr.ComparePrimOp(opKind, operandType) if resolveComparePrimOp(opKind, translateBaseType(operandType)).isDefined =>
        val instr = resolveComparePrimOp(opKind, translateBaseType(operandType)).get
        argInstrs ++ List(instr)
      // bool ops: &&, ||
      case op if Expr.isBoolAnd(op) =>
        val arg1 :: arg2 :: Nil = args: @unchecked
        translateBranching(arg1, genTerm(arg2), List(Instruction.I32Const(0)), ValType.I32)
      case op if Expr.isBoolOr(op) =>
        val arg1 :: arg2 :: Nil = args: @unchecked
        translateBranching(arg1, List(Instruction.I32Const(1)), genTerm(arg2), ValType.I32)
      // bool ops: not
      case Expr.UnaryPrimOp(Expr.BasicPrimOpKind.Not, Expr.BaseType.BoolType) => argInstrs ++ List(Instruction.I32Eqz)
      // ops: println, read
      case Expr.I32Println() => argInstrs ++ List(
        Instruction.Call(Symbol.I32Println),
        Instruction.I32Const(0)
      )
      case Expr.PutChar() =>
        val arg :: Nil = args: @unchecked
        val argInstrs = genTerm(arg)
        argInstrs ++ List(
          Instruction.Call(Symbol.PutChar),
          Instruction.I32Const(0)
        )
      case Expr.UnsafeAsPure() => args.flatMap(genTerm)
      case Expr.I32Read() => argInstrs ++ List(Instruction.Call(Symbol.I32Read))
      case Expr.PerfCounter() => argInstrs ++ List(Instruction.Call(Symbol.PerfCounter))
      case Expr.Box() | Expr.Unbox() => args.flatMap(genTerm)  // box and unbox have no runtime effects
      case Expr.StructSet() =>
        val Expr.Term.Select(base, fieldInfo) :: rhs :: Nil = args: @unchecked
        val rhsInstrs = genTerm(rhs)
        base.tpe.simplify(using ctx.typecheckerCtx).strip match
          case AppliedStructType(classSym, typeArgs) =>
            val StructInfo(structSym, fieldMap, _, _) = createStructType(classSym, typeArgs)
            val fieldSym = fieldMap(fieldInfo.name)
            val baseInstrs = genTerm(base)
            val setFieldInstrs = List(Instruction.StructSet(structSym, fieldSym))
            val unitInstrs = List(Instruction.I32Const(0))
            baseInstrs ++ rhsInstrs ++ setFieldInstrs ++ unitInstrs
          case AppliedStructTypeOnArena(classSym, typeArgs) =>
            val structInfo = createStructType(classSym, typeArgs)
            val baseInstrs = genTerm(base)
            val rhsInstrs = genTerm(rhs)
            val unitInstrs = List(Instruction.I32Const(0))
            baseInstrs ++ genArenaStructSet(structInfo, fieldInfo, rhsInstrs) ++ unitInstrs
          case _ => assert(false, s"Unsupported type: ${base.tpe}")
      case Expr.Sorry() =>
        // Generate `sorry` as `unreachable`, which is a runtime trap
        List(Instruction.Unreachable)
      case _ => assert(false, s"Not supported: $op")

  def translateBranching(cond: Expr.Term, thenBranch: List[Instruction], elseBranch: List[Instruction], resultType: ValType)(using Context): List[Instruction] =
    cond match
      case Term.PrimOp(op, _, arg1 :: arg2 :: Nil) if Expr.isBoolAnd(op) =>
        val cond1Instrs = genTerm(arg1)
        val moreInstrs = translateBranching(arg2, thenBranch, elseBranch, resultType)
        cond1Instrs ++ List(Instruction.If(resultType, moreInstrs, elseBranch))
      case Term.PrimOp(op, _, arg1 :: arg2 :: Nil) if Expr.isBoolOr(op) =>
        val cond1Instrs = genTerm(arg1)
        val moreInstrs = translateBranching(arg2, thenBranch, elseBranch, resultType)
        cond1Instrs ++ List(Instruction.If(resultType, moreInstrs, elseBranch))
      case cond =>
        val condInstrs = genTerm(cond)
        condInstrs ++ List(Instruction.If(resultType, thenBranch, elseBranch))

  def nameEncode(name: String): String =
    name.replaceAll(" ", "_").filter(ch => ch != '(' && ch != ')')

  def translateParamTypes(params: List[Expr.Binder.TermBinder])(using Context): List[ValType] =
    params match
      case Nil => Nil
      case p :: ps => 
        val paramType = translateType(p.tpe)
        val ctx1 = ctx.withMoreBinderInfos(BinderInfo.Inaccessible(p) :: Nil)
        paramType :: translateParamTypes(ps)(using ctx1)

  def computePolyFuncType(tpe: Expr.Type, typeBinderInfos: List[BinderInfo], isClosure: Boolean = true)(using Context): FuncType =
    val Expr.Type.TypeArrow(tparams, result) = tpe.strip: @unchecked
    val paramTypes =
      if isClosure then
        ValType.AnyRef :: Nil
      else
        Nil
    val resultType = translateType(result)(using ctx.withMoreBinderInfos(typeBinderInfos))
    FuncType(paramTypes, Some(resultType))

  def computeFuncType(tpe: Expr.Type, isClosure: Boolean = true)(using Context): FuncType = tpe.simplify(using ctx.typecheckerCtx) match
    case Expr.Type.TermArrow(params, result) =>
      var paramTypes = translateParamTypes(params)
      if isClosure then
        // the first param is the closure pointer
        paramTypes = ValType.AnyRef :: paramTypes
      val paramBinderInfos = params.map: binder =>
        BinderInfo.Inaccessible(binder)
      FuncType(paramTypes, Some(translateType(result)(using ctx.withMoreBinderInfos(paramBinderInfos))))
    case Expr.Type.TypeArrow(tparams, result) =>
      val paramTypes =
        if isClosure then
          ValType.AnyRef :: Nil
        else
          Nil
      val paramBinderInfos = tparams.map: binder =>
        BinderInfo.Abstract(binder)
      val resultType = translateType(result)(using ctx.withMoreBinderInfos(paramBinderInfos))
      FuncType(paramTypes, Some(resultType))
    case Expr.Type.Capturing(inner, _, _) => computeFuncType(inner, isClosure)
    case _ => assert(false, s"Unsupported type for computing func type: $tpe")

  def createFuncParams(params: List[Expr.Binder.TermBinder])(using Context): List[(Symbol, ValType)] =
    val paramTypes = translateParamTypes(params)
    (params `zip` paramTypes).map: (binder, paramType) =>
      val paramName = binder.name
      (Symbol.fresh(paramName), paramType)
  
  def newLocalsScope[R](op: Context ?=> R)(using Context): R =
    val oldLocals = ctx.locals
    val ctx1 = ctx.copy(locals = ArrayBuffer.empty)
    op(using ctx1)

  def createClosureTypes(funcType: FuncType)(using Context): ClosureTypeInfo =
    ctx.closureTypes.get(funcType) match
      case None => 
        val typeName = 
          nameEncode(funcType.paramTypes.map(_.show).mkString("_") + " to " + funcType.resultType.get.show)
        val closName = s"clos_$typeName"
        val funcSymm = Symbol.fresh(typeName)
        val closSymm = Symbol.fresh(closName)
        val closType = StructType(
          List(
            FieldType(Symbol.Function, ValType.TypedRef(funcSymm), mutable = false),
          ),
          subClassOf = None
        )
        emitType(TypeDef(funcSymm, funcType))
        emitType(TypeDef(closSymm, closType))
        val result = ClosureTypeInfo(funcSymm, closSymm)
        ctx.closureTypes += (funcType -> result)
        result
      case Some(info) => info

  /** Declare an array type in the type section; if exists, return the existing symbol */
  def createArrayType(arrType: ArrayType)(using Context): Symbol =
    ctx.arrayTypes.get(arrType) match
      case None => 
        val elemTypeStr = arrType.elemType.show
        val mutStr = if arrType.mutable then "mut_" else ""
        val typeName = nameEncode(s"array_of_${mutStr}${elemTypeStr}")
        val typeSym = Symbol.fresh(typeName)
        emitType(TypeDef(typeSym, arrType))
        ctx.arrayTypes += (arrType -> typeSym)
        typeSym
      case Some(symbol) => symbol

  def computeArrayType(tpe: Expr.Type)(using Context): ArrayType = 
    tpe.dealiasTypeVar.strip.simplify(using ctx.typecheckerCtx) match
      case PrimArrayType(elemType) => 
        ArrayType(translateType(elemType), mutable = true)
      case _ => assert(false, s"Unsupported type: $tpe")

  def translateBaseType(tpe: Expr.BaseType)(using Context): ValType =
    tpe match
      case Expr.BaseType.I64 => ValType.I64
      case Expr.BaseType.I32 => ValType.I32
      case Expr.BaseType.F64 => ValType.F64
      case Expr.BaseType.UnitType => ValType.I32
      case Expr.BaseType.BoolType => ValType.I32
      case Expr.BaseType.CharType => ValType.I32
      case Expr.BaseType.AnyType => ValType.AnyRef
      case Expr.BaseType.NothingType => ValType.AnyRef
      case Expr.BaseType.ArenaType => ValType.I32
      case _ => assert(false, s"Unsupported base type: $tpe")

  /** What is the WASM value type of the WASM representation of a value of this type? */
  def translateType(tpe: Expr.Type)(using Context): ValType = //trace(s"translateType($tpe)"):
    tpe.simplify(using ctx.typecheckerCtx) match
      case Expr.Type.Base(baseType) => translateBaseType(baseType)
      case Expr.Type.Capturing(inner, _, _) => translateType(inner)
      case Expr.Type.Boxed(core) => translateType(core)
      case Expr.Type.TermArrow(params, result) =>
        val funcType = computeFuncType(tpe)
        val info = createClosureTypes(funcType)
        ValType.TypedRef(info.closTypeSym)
      case Expr.Type.TypeArrow(tparams, result) =>
        val funcType = computeFuncType(tpe)
        val info = createClosureTypes(funcType)
        ValType.TypedRef(info.closTypeSym)
      case PrimArrayType(elemType) => 
        val arrType = computeArrayType(tpe)
        val arrSym = createArrayType(arrType)
        ValType.TypedRef(arrSym)
      case AppliedStructType(sym, targs) =>
        val StructInfo(structSym, _, _, _) = createStructType(sym, targs)
        ValType.TypedRef(structSym)
      case AppliedStructTypeOnArena(sym, targs) => 
        // Arena-allocated structs are simply a pointer to the linear memory
        ValType.I32
      case AppliedEnumTypeOnArena(sym, targs) =>
        // Arena-allocated enums are simply a pointer to the linear memory
        ValType.I32
      case AppliedEnumType(sym, targs) => ValType.TypedRef(Symbol.EnumClass)
      case Expr.Type.BinderRef(idx) =>
        //println(s"translateType: BinderRef($idx), binderInfos = ${ctx.binderInfos}")
        ctx.binderInfos(idx) match
          case BinderInfo.Specialized(_, tpe) => tpe
          case BinderInfo.Abstract(_) => ValType.AnyRef
          case info => assert(false, s"This is absurd, idx = $idx, binderInfos = ${ctx.binderInfos}, info = $info")
      case Expr.Type.RefinedType(base, _) => translateType(base)
      case _ => assert(false, s"Unsupported type: $tpe")

  def dropLocalBinders(xs: Set[Int], numLocals: Int): Set[Int] =
    xs.flatMap: idx =>
      if idx >= numLocals then
        Some(idx - numLocals)
      else
        None

  def freeLocalBinders(t: Expr.Term)(using Context): Set[Int] = t match
    case Term.BinderRef(idx) => Set(idx)
    case Term.SymbolRef(sym) => Set.empty
    case Term.IntLit(value) => Set.empty
    case Term.FloatLit(value) => Set.empty
    case Term.StrLit(value) => Set.empty
    case Term.CharLit(value) => Set.empty
    case Term.BoolLit(value) => Set.empty
    case Term.UnitLit() => Set.empty
    case Term.TermLambda(params, body, _) =>
      dropLocalBinders(freeLocalBinders(body), params.size)
    case Term.TypeLambda(params, body) =>
      dropLocalBinders(freeLocalBinders(body), params.size)
    case Term.Bind(binder, recursive, bound, body) =>
      val boundFree = freeLocalBinders(bound)
      val boundFree1 = if recursive then dropLocalBinders(boundFree, 1) else boundFree
      val bodyFree = dropLocalBinders(freeLocalBinders(body), 1)
      boundFree1 ++ bodyFree
    case Term.PrimOp(op, _, args) => args.flatMap(freeLocalBinders).toSet
    case Term.Apply(fun, args) => freeLocalBinders(fun) ++ args.flatMap(freeLocalBinders)
    case Term.TypeApply(term, targs) => freeLocalBinders(term)
    case Term.Select(base, fieldInfo) => freeLocalBinders(base)
    case Term.StructInit(sym, _, args) => args.flatMap(freeLocalBinders).toSet
    case Term.If(cond, thenBranch, elseBranch) =>
      freeLocalBinders(cond) ++ freeLocalBinders(thenBranch) ++ freeLocalBinders(elseBranch)
    case Term.ResolveExtension(sym, targs, field) => Set.empty
    case Term.Match(scrutinee, cases) =>
      freeLocalBinders(scrutinee) ++ cases.flatMap: cas =>
        val bodyFree = freeLocalBinders(cas.body)
        val binders = TypeChecker.bindersInPattern(cas.pat)
        dropLocalBinders(bodyFree, binders.size)

  /** Translate a closure of `funType` with a given parameter list and body.
   * Returns the instructions for creating the closure and the symbol of the worker function.
   */
  def genClosure(
    funType: Expr.Type,   // the type of the source function
    params: List[Expr.Binder],   // parameters of the source function
    body: Expr.Term,   // body of the source function
    selfBinder: Option[Expr.Binder] = None,  // whether the source function is self-recursive
    typeBinderInfos: Option[List[BinderInfo]] = None  // specialised type arguments
  )(using Context): (List[Instruction], Symbol) =
    val isTypeLambda = funType.strip match
      case Expr.Type.TypeArrow(_, _) => true
      case _ => false
    val funcType =
      typeBinderInfos match
        case Some(infos) => computePolyFuncType(funType, infos, isClosure = true)
        case None => computeFuncType(funType, isClosure = true)
    val closureInfo = createClosureTypes(funcType)
    val funName: String = selfBinder match
      case Some(bd) => bd.name
      case None => "anonfun"
    val freeVars = dropLocalBinders(freeLocalBinders(body), params.size)
    // (1) create the exact closure type
    val depVarsSet = 
      // local variables this closure depends on other than itself
      if selfBinder.isDefined then dropLocalBinders(freeVars, 1) else freeVars
    val depVars = depVarsSet.toList
    val envMap: Map[Int, (Symbol, ValType)] = Map.from:
      depVars.map: idx =>
        val binder = ctx.binderInfos(idx).binder.asInstanceOf[Expr.Binder.TermBinder]
        val sym = Symbol.fresh(binder.name)
        val tpe = translateType(binder.tpe.shift(idx + 1))
        (idx, (sym, tpe))
    val fields = 
      FieldType(Symbol.Function, ValType.TypedRef(closureInfo.funcTypeSym), mutable = false) :: depVars.map: idx =>
        val (sym, tpe) = envMap(idx)
        FieldType(sym, tpe, mutable = false)
    val exactClosureType = StructType(fields, subClassOf = Some(closureInfo.closTypeSym))
    val exactClosureTypeName = s"clos_${funName}"
    val exactClosureTypeSym = Symbol.fresh(exactClosureTypeName)
    emitType(TypeDef(exactClosureTypeSym, exactClosureType))
    // (2) create the worker function
    val workerName = s"worker_${funName}"
    val workerSym = Symbol.fresh(workerName)
    val selfParamSym = Symbol.fresh("self")
    val selfCastedSym = Symbol.fresh("self_casted")
    val localMap: Map[Int, Symbol] = Map.from:
      depVars.map: idx =>
        val binder = ctx.binderInfos(idx).binder.asInstanceOf[Expr.Binder.TermBinder]
        val sym = Symbol.fresh(s"local_${binder.name}")
        (idx, sym)
    def funcLocals = depVars.map: idx =>
      val tpe = envMap(idx)._2
      val sym = localMap(idx)
      (sym, tpe)
    val selfCastInstrs = List(
      Instruction.LocalGet(selfParamSym),
      Instruction.RefCast(exactClosureTypeSym),
      Instruction.LocalSet(selfCastedSym)
    )
    val setLocalInstrs = depVars.flatMap: idx =>
      List(
        Instruction.LocalGet(selfCastedSym),
        Instruction.StructGet(exactClosureTypeSym, envMap(idx)._1),
        Instruction.LocalSet(localMap(idx))
      )
    val localCtx = selfBinder match
      case Some(binder) => ctx.withLocalSym(binder, selfCastedSym)
      case None => ctx
    val workerFunParams = 
      if !isTypeLambda then
        (selfParamSym -> ValType.AnyRef) :: createFuncParams(params.asInstanceOf[List[Expr.Binder.TermBinder]])(using localCtx)
      else
        (selfParamSym -> ValType.AnyRef) :: Nil
    val workerFunc: Wasm.Func =
      newLocalsScope:
        emitLocal(selfCastedSym, ValType.TypedRef(exactClosureTypeSym))
        emitLocals(funcLocals)
        val newBinderInfos: List[BinderInfo] = ctx.binderInfos.zipWithIndex.map: (binderInfo, idx) =>
          if depVarsSet `contains` idx then
            BinderInfo.Sym(binderInfo.binder, localMap(idx))
          else
            binderInfo match
              case i: (BinderInfo.Specialized | BinderInfo.Abstract) => i
              case _ => BinderInfo.Inaccessible(binderInfo.binder)
        var ctx1 = ctx.usingBinderInfos(newBinderInfos)
        selfBinder match
          case Some(binder) =>
            ctx1 = ctx1.withClosureSym(binder, selfCastedSym, workerSym)
          case None =>
        if !isTypeLambda then
          (params `zip` workerFunParams.tail).foreach: 
            case (binder, (sym, _)) =>
              ctx1 = ctx1.withLocalSym(binder, sym)
        else
          val binderInfos = typeBinderInfos.getOrElse:
            params.map: bd =>
              BinderInfo.Abstract(bd)
          ctx1 = ctx1.withMoreBinderInfos(binderInfos)
        val bodyInstrs = genTerm(body)(using ctx1)
        val bodyInstrs1 = maybeAddReturnCall(bodyInstrs)
        val funcResultType = funcType.resultType
        Func(
          workerSym, 
          workerFunParams, 
          funcResultType,
          locals = finalizeLocals,
          body = selfCastInstrs ++ setLocalInstrs ++ bodyInstrs1
        )
    emitFunc(workerFunc)
    emitElemDeclare(ExportKind.Func, workerSym)
    val getFuncInstrs = List(Instruction.RefFunc(workerSym))
    val getEnvInstrs = depVars.flatMap: idx =>
      genBinderRef(idx)
    val createStructInstrs = List(
      Instruction.StructNew(exactClosureTypeSym)
    )
    (getFuncInstrs ++ getEnvInstrs ++ createStructInstrs, workerSym)

  def genBinderRef(binder: Int)(using Context): List[Instruction] =
    ctx.binderInfos(binder) match
      case BinderInfo.Sym(binder, sym) => List(Instruction.LocalGet(sym))
      case BinderInfo.ClosureSym(_, sym, _) => List(Instruction.LocalGet(sym))
      case BinderInfo.Erased(binder) => List(Instruction.I32Const(0))
      case BinderInfo.Inaccessible(binder) => assert(false, s"Inaccessible binder: $binder")
      case BinderInfo.PolyClosureSym(_, _, _, _) => 
        assert(false, "Type-polymorphic function cannot be used as a first-class value. This is a restriction in the code generator.")
      case _ => assert(false, "absurd binder info")

  def isClosureSym(binderIdx: Int)(using Context): Boolean =
    ctx.binderInfos(binderIdx) match
      case _: BinderInfo.ClosureSym => true
      case _ => false

  def isPolyClosureSym(binderIdx: Int)(using Context): Boolean =
    ctx.binderInfos(binderIdx) match
      case _: BinderInfo.PolyClosureSym => true
      case _ => false

  def createPolyClosure(
    typeArgs: List[Expr.Type | Expr.CaptureSet],
    info: BinderInfo.PolyClosureSym
  )(using Context): (List[Instruction], (Symbol, Symbol)) =
    val specSig = translateTypeArgs(typeArgs)
    info.specMap.get(specSig) match
      case Some(res) => (Nil, res)
      case None =>
        val typeBinderInfos = (info.params `zip` typeArgs).map:
          case (param: Expr.Binder.TypeBinder, typeArg: Expr.Type) =>
            BinderInfo.Specialized(param, translateType(typeArg))
          case (param: Expr.Binder.CaptureBinder, typeArg: Expr.CaptureSet) =>
            BinderInfo.Inaccessible(param)
          case _ => assert(false)
        val funcSym = Symbol.fresh("polyclos")
        val funcType = computePolyFuncType(info.binder.tpe, typeBinderInfos, isClosure = true)
        val closureInfo = createClosureTypes(funcType)
        emitLocal(funcSym, ValType.TypedRef(closureInfo.closTypeSym))
        val (instrs, workerSym) = info.generator(typeBinderInfos)
        val outInstrs = instrs ++ List(Instruction.LocalSet(funcSym))
        (outInstrs, (funcSym, workerSym))

  def genTerm(t: Expr.Term)(using Context): List[Instruction] = t match
    case Term.IntLit(value) => 
      translateType(t.tpe) match
        case ValType.I32 => List(Instruction.I32Const(value))
        case ValType.I64 => List(Instruction.I64Const(value))
        case _ => assert(false, s"Unsupported type for int literal: ${t.tpe}")
    case Term.FloatLit(value) =>
      translateType(t.tpe) match
        case ValType.F64 => List(Instruction.F64Const(value))
        case _ => assert(false, s"Unsupported type for float literal: ${t.tpe}")
    case Term.UnitLit() => List(Instruction.I32Const(0))
    case Term.CharLit(value) => List(Instruction.I32Const(value.toInt))
    case Term.BoolLit(value) => if value then List(Instruction.I32Const(1)) else List(Instruction.I32Const(0))
    case Term.StrLit(value) => 
      val typeSym = createArrayType(ArrayType(ValType.I32, mutable = true))
      val size = value.length
      val elemInstrs: List[Instruction] = value.map(ch => Instruction.I32Const(ch.toInt)).toList
      elemInstrs ++ List(Instruction.ArrayNewFixed(typeSym, size))
    case Term.PrimOp(arrayOp: Expr.ArrayPrimitiveOp, targs, args) =>
      args match
        case Term.SymbolRef(sym) :: _ if sym eq Expr.Definitions.MemorySymbol => 
          genMemoryOp(arrayOp, args)
        case _ =>
          genArrayPrimOp(arrayOp, t.tpe, targs, args)
    case Term.PrimOp(arenaOp: Expr.ArenaPrimitiveOp, targs, args) =>
      (arenaOp, targs, args) match
        case (Expr.Arena(), resType :: Nil, (body: Term.TermLambda) :: Nil) =>
          genArena(body, resType)
        case (Expr.RegionAlloc(), Nil, _ :: (initTerm: Term.StructInit) :: Nil) =>
          genArenaStructInit(initTerm.sym, initTerm.targs, initTerm.args)
        case _ => assert(false, s"unsupported arena primitive op: $t")
    case Term.PrimOp(op, Nil, args) => genSimplePrimOp(args, op)
    case Term.If(cond, thenBranch, elseBranch) =>
      val resultType = translateType(t.tpe)
      val then1 = genTerm(thenBranch)
      val else1 = genTerm(elseBranch)
      translateBranching(cond, then1, else1, resultType)
    case Term.TermLambda(params, body, _) =>
      genClosure(t.tpe, params, body, selfBinder = None)._1
    case Term.TypeLambda(params, body) =>
      genClosure(t.tpe, params, body, selfBinder = None)._1
    case Term.Bind(binder, isRecursive, clos: Expr.Closure, expr) =>
      val (params, body, isTypeLambda) = clos match
        case Term.TermLambda(params, body, _) => (params, body, false)
        case Term.TypeLambda(params, body) => (params, body, true)
      if clos.isTypePolymorphic then
        def generator(typeBinderInfos: List[BinderInfo]): (List[Instruction], Symbol) =
          genClosure(binder.tpe, params, body, if isRecursive then Some(binder) else None, Some(typeBinderInfos))
        val ctx1 = ctx.withPolyClosureSym(binder, params, generator)
        val bodyInstrs = genTerm(expr)(using ctx1)
        bodyInstrs
      else
        val localSym = Symbol.fresh(binder.name)
        val localType = translateType(binder.tpe)
        emitLocal(localSym, localType)
        val (closureInstrs, workerSym) = genClosure(binder.tpe, params, body, if isRecursive then Some(binder) else None)
        val setLocalInstrs = List(Instruction.LocalSet(localSym))
        val bodyInstrs = genTerm(expr)(using ctx.withClosureSym(binder, localSym, workerSym))
        closureInstrs ++ setLocalInstrs ++ bodyInstrs
    case Term.Bind(binder, false, bound, body) =>
      val localSym = Symbol.fresh(binder.name)
      val localType = translateType(binder.tpe)
      emitLocal(localSym, localType)
      val boundInstrs = genTerm(bound)
      val bodyInstrs = genTerm(body)(using ctx.withLocalSym(binder, localSym))
      boundInstrs ++ List(Instruction.LocalSet(localSym)) ++ bodyInstrs
    case Term.BinderRef(idx) => genBinderRef(idx)
    case Term.SymbolRef(sym) =>
      val info = ctx.defInfos(sym)
      info match
        case info: (DefInfo.FuncDef | DefInfo.LazyFuncDef) =>
          val DefInfo.FuncDef(_, workerSym) = createModuleFunction(sym)
          val funcType = computeFuncType(t.tpe, isClosure = true)
          val closureInfo = createClosureTypes(funcType)
          val getSelfInstrs = List(
            Instruction.RefFunc(workerSym),
          )
          val createClosureInstrs = List(
            Instruction.StructNew(closureInfo.closTypeSym),
          )
          getSelfInstrs ++ createClosureInstrs
        case DefInfo.GlobalDef(globalSym) =>
          List(Instruction.GlobalGet(globalSym))
        case _: DefInfo.PolyFuncDef =>
          assert(false, "Type-polymorphic def cannot be used as first-class value. This is a restriction in the code generator.")
    case Term.Apply(Term.SymbolRef(sym), args) =>
      val DefInfo.FuncDef(funcSym, _) = createModuleFunction(sym)
      val argInstrs = args.flatMap(genTerm)
      val callInstrs = List(Instruction.Call(funcSym))
      argInstrs ++ callInstrs
    case Term.TypeApply(Term.SymbolRef(sym), typeArgs) if isPolySymbol(sym) =>
      val DefInfo.FuncDef(funcSym, _) = createPolyModuleFunction(sym, typeArgs)
      val callInstrs = List(Instruction.Call(funcSym))
      callInstrs
    case Term.TypeApply(Term.SymbolRef(sym), _) =>
      val DefInfo.FuncDef(funcSym, _) = createModuleFunction(sym)
      val callInstrs = List(Instruction.Call(funcSym))
      callInstrs
    case Term.Apply(Term.ResolveExtension(extSym, targs, field), args) =>
      val ExtensionInfo(methodMap) = createExtensionInfo(extSym, targs)
      val methodInfo = methodMap(field)
      val methodSym = methodInfo match
        case MethodInfo.SimpleMethod(sym, _) => sym
        case _: MethodInfo.PolyMethod => assert(false)
      val argInstrs = args.flatMap(genTerm)
      val callInstrs = List(Instruction.Call(methodSym))
      argInstrs ++ callInstrs
    case Term.TypeApply(Term.ResolveExtension(extSym, targs, field), typeArgs) =>
      val extInfo = createExtensionInfo(extSym, targs)
      val MethodInfo.SimpleMethod(sym, _) = getPolyMethodInfo(extInfo, field, typeArgs)
      val callInstrs = List(Instruction.Call(sym))
      callInstrs
    case Term.Apply(fun @ Term.BinderRef(idx), args) if isClosureSym(idx) =>
      val BinderInfo.ClosureSym(_, sym, funSym) = ctx.binderInfos(idx): @unchecked
      val getSelfInstrs = genBinderRef(idx)
      val argInstrs = args.flatMap(genTerm)
      val callWorkerInstrs = List(Instruction.Call(funSym))
      getSelfInstrs ++ argInstrs ++ callWorkerInstrs
    case Term.TypeApply(Term.BinderRef(idx), typeArgs) if isPolyClosureSym(idx) =>
      val info = ctx.binderInfos(idx).asInstanceOf[BinderInfo.PolyClosureSym]
      val (instrs, (funcSym, workerSym)) = createPolyClosure(typeArgs, info)
      val getSelfInstrs = List(Instruction.LocalGet(funcSym))
      val callWorkerInstrs = List(Instruction.Call(workerSym))
      instrs ++ getSelfInstrs ++ callWorkerInstrs
    case Term.Apply(fun, args) =>
      val localSym = Symbol.fresh("fun")
      val funcType = computeFuncType(fun.tpe)
      val info = createClosureTypes(funcType)
      emitLocal(localSym, ValType.TypedRef(info.closTypeSym))
      val funInstrs = genTerm(fun) ++ List(Instruction.LocalSet(localSym))
      val getWorkerInstrs = List(
        Instruction.LocalGet(localSym),
        Instruction.StructGet(info.closTypeSym, Symbol.Function),
      )
      val getSelfArgInstrs = List(Instruction.LocalGet(localSym))
      val argInstrs = args.flatMap(genTerm)
      val callRefInstrs = List(Instruction.CallRef(info.funcTypeSym))
      funInstrs ++ getSelfArgInstrs ++ argInstrs ++ getWorkerInstrs ++ callRefInstrs
    case Term.TypeApply(fun, targs) =>
      val localSym = Symbol.fresh("fun")
      val funcType = computeFuncType(fun.tpe)
      val info = createClosureTypes(funcType)
      emitLocal(localSym, ValType.TypedRef(info.closTypeSym))
      val funInstrs = genTerm(fun) ++ List(Instruction.LocalSet(localSym))
      val getWorkerInstrs = List(
        Instruction.LocalGet(localSym),
        Instruction.StructGet(info.closTypeSym, Symbol.Function),
      )
      val getSelfArgInstrs = List(Instruction.LocalGet(localSym))
      val callRefInstrs = List(Instruction.CallRef(info.funcTypeSym))
      funInstrs ++ getSelfArgInstrs ++ getWorkerInstrs ++ callRefInstrs
    case Term.StructInit(classSym, targs, args) =>
      genStructInit(classSym, targs, args)
    case Term.Select(base, fieldInfo) =>
      genSelect(base, fieldInfo)
    case Term.Match(scrutinee, cases) =>
      genMatch(scrutinee, cases, t.tpe)
    case _ => assert(false, s"Don't know how to translate this term: $t")

  /** Generate code for a pattern.
   * Assume that the scrutinee is on the top of the stack.
   * Generates instructions for getting the fields of each pattern along with the corresponding binder infos.
   */
  def genPatternExtractor(pat: Expr.Pattern)(using Context): (List[Instruction], List[BinderInfo]) =
    pat match
      case Pattern.Wildcard() =>
        (List(Instruction.Drop), Nil)
      case Pattern.Bind(binder, pat) =>
        val valType = translateType(binder.tpe)
        val localSym = Symbol.fresh(binder.name)
        emitLocal(localSym, valType)
        val (patInstrs, patBinderInfos) = genPatternExtractor(pat)
        val bindInstr = Instruction.LocalTee(localSym)
        val localBinderInfo = BinderInfo.Sym(binder, localSym)
        (bindInstr :: patInstrs, localBinderInfo :: patBinderInfos)
      case Pattern.EnumVariant(constructor, typeArgs, enumSym, fields) =>
        val onArena = pat.tpe.strip.isOnArena(using ctx.typecheckerCtx)
        val structInfo = createStructType(constructor, typeArgs)
        val StructInfo(structSym, fieldMap, _, _) = structInfo
        val localSym = Symbol.fresh(constructor.name)
        val valType = 
          if onArena then
            ValType.I32
          else
            ValType.TypedRef(structSym)
        emitLocal(localSym, valType)
        val setLocalInstrs =
          enumSym match
            case Some(_) if !onArena =>
              List(
                Instruction.RefCast(structSym),
                Instruction.LocalSet(localSym),
              )
            case _ => List(Instruction.LocalSet(localSym))
        val fieldResults = (constructor.info.fields `zip` fields).map: (fieldInfo, field) =>
          val (instrs, binders) = genPatternExtractor(field)
          val fieldName = fieldInfo.name
          val getFieldInstrs = 
            if !onArena then
              List(
                Instruction.LocalGet(localSym),
                Instruction.StructGet(structSym, fieldMap(fieldName)),
              )
            else
              Instruction.LocalGet(localSym) :: genArenaStructGet(structInfo, fieldInfo)
          (getFieldInstrs ++ instrs, binders)
        val (instrs, binders) = fieldResults.unzip
        (setLocalInstrs ++ instrs.flatten, binders.flatten)

  /** Generate WASM instructions for checking whether the scrutinee matches the pattern.
   * Assume that the scrutinee is on the top of the stack.
   * It will always consume that scrutinee and enter either `thenBranch` or `elseBranch` based on the result.
   */
  def genPatternMatcher(pat: Expr.Pattern, thenBranch: List[Instruction], elseBranch: List[Instruction], resType: ValType)(using Context): List[Instruction] =
    pat match
      case Pattern.Wildcard() => Instruction.Drop :: thenBranch
      case Pattern.Bind(binder, pat) => genPatternMatcher(pat, thenBranch, elseBranch, resType)
      case Pattern.EnumVariant(constructor, typeArgs, enumSym, fields) =>
        val onArena = pat.tpe.strip.isOnArena(using ctx.typecheckerCtx)
        val structInfo = createStructType(constructor, typeArgs)
        val StructInfo(structSym, fieldMap, _, maybeTag) = structInfo
        val localSym = Symbol.fresh(constructor.name)
        val localType = 
          if onArena then
            ValType.I32
          else
            ValType.TypedRef(structSym)
        emitLocal(localSym, localType)
        val setLocalInstrs = List(Instruction.LocalSet(localSym))
        val getLocalInstr = Instruction.LocalGet(localSym)
        def go(fields: List[(Expr.FieldInfo, Expr.Pattern)]): List[Instruction] = fields match
          case (fieldInfo, field) :: rest =>
            val fieldSym = fieldMap(fieldInfo.name)
            val projFieldInstrs = 
              if !onArena then
                List(
                  getLocalInstr,
                  Instruction.StructGet(structSym, fieldSym),
                )
              else
                getLocalInstr :: genArenaStructGet(structInfo, fieldInfo)
            projFieldInstrs ++ genPatternMatcher(field, go(rest), elseBranch, resType)
          case Nil => thenBranch
        val fieldInstrs = go(constructor.info.fields `zip` fields)
        enumSym match
          case None => setLocalInstrs ++ fieldInstrs
          case Some(_) =>
            val originalSym = Symbol.fresh("original")
            val originalType = if onArena then ValType.I32 else ValType.TypedRef(Symbol.EnumClass)
            emitLocal(originalSym, originalType)
            val getTagInstrs =
              if !onArena then
                List(Instruction.StructGet(Symbol.EnumClass, Symbol.Tag))
              else genArenaStructGet(structInfo, Symbol.Tag)
            val testInstrs = 
              List(
                Instruction.LocalTee(originalSym)
              ) ++ getTagInstrs ++ List(
                Instruction.I32Const(maybeTag.get),
                Instruction.I32Eq,
              )
            val castInstrs = 
              if !onArena then
                List(Instruction.LocalGet(originalSym), Instruction.RefCast(structSym))
              else 
                List(Instruction.LocalGet(originalSym))
            val ifInstr = Instruction.If(
              resType,
              thenBranch = castInstrs ++ setLocalInstrs ++ fieldInstrs,
              elseBranch = elseBranch,
            )
            testInstrs ++ List(ifInstr)

  def genMatch(scrutinee: Expr.Term, cases: List[Expr.MatchCase], tpe: Expr.Type)(using Context): List[Instruction] =
    val resType = translateType(tpe)
    val scrutineeInstrs = genTerm(scrutinee)
    val localSym = Symbol.fresh("match_scrutinee")
    val localType = translateType(scrutinee.tpe)
    emitLocal(localSym, localType)
    val setLocalInstrs = List(Instruction.LocalSet(localSym))
    def go(cases: List[Expr.MatchCase]): List[Instruction] = cases match
      case Nil => List(Instruction.Unreachable)
      case cas :: rest =>
        val getLocalInstrs = List(Instruction.LocalGet(localSym))
        val (patInstrs, patBinderInfos) = genPatternExtractor(cas.pat)
        val bodyInstrs = genTerm(cas.body)(using ctx.withMoreBinderInfos(patBinderInfos))
        val thenBranch = getLocalInstrs ++ patInstrs ++ bodyInstrs
        val elseBranch = go(rest)
        val matcherInstrs = genPatternMatcher(cas.pat, thenBranch, elseBranch, resType)
        getLocalInstrs ++ matcherInstrs
    val caseInstrs = go(cases)
    scrutineeInstrs ++ setLocalInstrs ++ caseInstrs

  def genStructInit(classSym: Expr.StructSymbol, targs: List[Expr.Type | Expr.CaptureSet], args: List[Expr.Term])(using Context): List[Instruction] =
    val StructInfo(structSym, _, _, tagNumber) = createStructType(classSym, targs)
    var argInstrs = args.flatMap(genTerm)
    tagNumber match
      case Some(num) =>
        argInstrs = Instruction.I32Const(num) :: argInstrs
      case None =>
    val createStructInstrs = List(Instruction.StructNew(structSym))
    argInstrs ++ createStructInstrs

  def memoryReprOf(valType: ValType): MemRepr = valType match
    case ValType.I32 => MemRepr.Plain(ValType.I32)
    case ValType.I64 => MemRepr.Plain(ValType.I64)
    case ValType.F64 => MemRepr.Plain(ValType.F64)
    case rtype: ReferenceType => MemRepr.Ref(rtype)

  def genArenaStructInit(classSym: Expr.StructSymbol, targs: List[Expr.Type | Expr.CaptureSet], args: List[Expr.Term])(using Context): List[Instruction] =
    val structSym = declareLocal("__struct", ValType.I32)
    val getStructPtrInstrs = List(Instruction.LocalGet(structSym))
    val structInfo = createStructType(classSym, targs)
    val setStructPtrInstrs = List(
      Instruction.GlobalGet(Symbol.ArenaCurrent),
      Instruction.LocalSet(structSym),
    )
    val incArenaInstrs = List(
      Instruction.GlobalGet(Symbol.ArenaCurrent),
      Instruction.I32Const(structInfo.layoutInfo.totalSize),
      Instruction.I32Add,
      Instruction.GlobalSet(Symbol.ArenaCurrent),
    )
    val setArgsInstr: List[Instruction] = (args `zip` classSym.info.fields).flatMap: (argTerm, fieldInfo) =>
      val argInstrs = genTerm(argTerm)
      getStructPtrInstrs ++ genArenaStructSet(structInfo, fieldInfo, argInstrs, isInitialising = true)
    val setTagInstrs = structInfo.tagNumber match
      case Some(tag) => getStructPtrInstrs ++ genArenaStructSet(structInfo, Symbol.Tag, List(Instruction.I32Const(tag)), isInitialising = true)
      case None => Nil
    setStructPtrInstrs ++ incArenaInstrs ++ setArgsInstr ++ setTagInstrs ++ getStructPtrInstrs

  /** Generate instructions for setting a field of an arena-allocated struct, assuming that the struct pointer is on the top of the stack. */
  def genArenaStructSet(structInfo: StructInfo, fieldInfo: Expr.FieldInfo | Symbol, getArg: List[Instruction], isInitialising: Boolean = false)(using Context): List[Instruction] =
    val StructInfo(_, nameMap, layoutInfo, _) = structInfo
    val FieldLayout(offset, memRepr) =
      fieldInfo match
        case fieldInfo: Expr.FieldInfo =>
          layoutInfo.fields(nameMap(fieldInfo.name))
        case fieldSym: Symbol =>
          layoutInfo.fields(fieldSym)
    val addressInstr = 
      if offset == 0 then
        Nil
      else
        List(
          Instruction.I32Const(offset),
          Instruction.I32Add,
        )
    memRepr match
      case MemRepr.Plain(tpe) =>
        val storeInstrs = List(
          Instruction.Store(memRepr.reprType, Symbol.ArenaMemory),
        )
        addressInstr ++ getArg ++ storeInstrs
      case MemRepr.Ref(refType) =>
        val builder = CodeBuilder()
        builder.emit(addressInstr)
        if isInitialising then
          // Allocate a shadow table entry for the struct
          val addrSym = declareLocal("__addr", ValType.I32)
          builder.emit(
            Instruction.LocalTee(addrSym),
            Instruction.GlobalGet(Symbol.ShadowTableCurrent),
            Instruction.Store(ValType.I32, Symbol.ArenaMemory),
            // Increment the shadow table pointer
            Instruction.GlobalGet(Symbol.ShadowTableCurrent),
            Instruction.I32Const(1),
            Instruction.I32Add,
            Instruction.GlobalSet(Symbol.ShadowTableCurrent),
            // Push the address back to the stack
            Instruction.LocalGet(addrSym),
          )
        // Get the shadow entry index
        builder.emit(Instruction.Load(ValType.I32, Symbol.ArenaMemory))
        builder.emit(getArg)
        // Set the entry
        builder.emit(Instruction.TableSet(Symbol.ShadowTable))
        builder.output

  def genSelect(base: Expr.Term, fieldInfo: Expr.FieldInfo)(using Context): List[Instruction] =
    base.tpe.strip.simplify(using ctx.typecheckerCtx) match
      case AppliedStructType(classSym, targs) =>
        val StructInfo(structSym, nameMap, _, _) = createStructType(classSym, targs)
        val fieldName = fieldInfo.name
        val fieldSym = nameMap(fieldName)
        val baseInstrs = genTerm(base)
        val getFieldInstrs = List(Instruction.StructGet(structSym, fieldSym))
        baseInstrs ++ getFieldInstrs
      case AppliedStructTypeOnArena(classSym, targs) =>
        val structInfo = createStructType(classSym, targs)
        val baseInstrs = genTerm(base)
        val getFieldInstrs = genArenaStructGet(structInfo, fieldInfo)
        baseInstrs ++ getFieldInstrs
      case _ => assert(false, "impossible, otherwise a bug in the typechecker")

  /** Generate instructions for retrieving a field from an arena-allocated struct, assuming that the struct pointer is on the top of the stack. */
  def genArenaStructGet(structInfo: StructInfo, fieldInfo: Expr.FieldInfo | Symbol)(using Context): List[Instruction] =
    val StructInfo(_, nameMap, layoutInfo, _) = structInfo
    val FieldLayout(offset, memRepr) = 
      fieldInfo match
        case fieldInfo: Expr.FieldInfo =>
          layoutInfo.fields(nameMap(fieldInfo.name))
        case fieldSym: Symbol =>
          layoutInfo.fields(fieldSym)
    val addressInstr = 
      if offset == 0 then
        Nil
      else
        List(
          Instruction.I32Const(offset),
          Instruction.I32Add,
        )
    memRepr match
      case MemRepr.Plain(tpe) =>
        val loadInstrs = List(
          Instruction.Load(memRepr.reprType, Symbol.ArenaMemory),
        )
        addressInstr ++ loadInstrs
      case MemRepr.Ref(refType) =>
        val builder = CodeBuilder()
        builder.emit(addressInstr)
        builder.emit(Instruction.Load(ValType.I32, Symbol.ArenaMemory))
        builder.emit(Instruction.TableGet(Symbol.ShadowTable))
        refType.getTypeSymbol match
          case Some(refTypeSym) =>
            builder.emit(Instruction.RefCast(refTypeSym))
          case _ =>
        builder.output

  def translateTypeArgs(targs: List[Expr.Type | Expr.CaptureSet])(using Context): SpecSig =
    val valTypes = targs.flatMap:
      case tpe: Expr.Type => Some(translateType(tpe))
      case _ => None
    SpecSig(valTypes)

  /** Number of bytes in the linear memory a value type takes. */
  def sizeInMemory(tpe: MemRepr): Int = tpe match
    case MemRepr.Plain(ValType.I32) => 4
    case MemRepr.Plain(ValType.I64) => 8
    case MemRepr.Plain(ValType.F64) => 8
    case MemRepr.Ref(_) => 4
    case _ => assert(false, s"malformed memory representation: $tpe")

  def createLayoutInfo(structType: StructType): LayoutInfo =
    val fields = structType.fields
    val fieldTypes = fields.map(t => memoryReprOf(t.tpe))
    val fieldSizes = fieldTypes.map(sizeInMemory)
    val totalSize = fieldSizes.sum
    val offsets = fieldSizes.scanLeft(0)(_ + _)
    val offsetsMap = Map.from(fields.map(_.sym).zip(offsets).zip(fieldTypes).map:
      case ((s: Symbol, o: Int), t: MemRepr) =>
        (s -> FieldLayout(o, t))
    )
    LayoutInfo(totalSize, offsetsMap)

  def createStructType(classSym: Expr.StructSymbol, targs: List[Expr.Type | Expr.CaptureSet])(using Context): StructInfo =
    val specSig = translateTypeArgs(targs)
    val specMap: mutable.Map[SpecSig, StructInfo] =
      ctx.typeInfos.get(classSym) match
        case None =>
          ctx.typeInfos += (classSym -> mutable.Map.empty)
          ctx.typeInfos.get(classSym).get
        case Some(m) => m
    specMap.get(specSig) match
      case Some(info) => info
      case None => 
        val structName = nameEncode(classSym.name ++ specSig.targs.map(vt => "_" ++ vt.show).mkString(""))
        val structSym = Symbol.fresh(structName)
        specMap += (specSig -> StructInfo(structSym, Map.empty, LayoutInfo.empty)) // first, put a placeholder
        val binders: List[Expr.Binder] = classSym.info.targs
        val binderInfos = (binders `zip` targs).map:
          case (bd, tpe: Expr.Type) => 
            val valType = translateType(tpe)
            BinderInfo.Specialized(bd, valType)
          case (bd, tpe: Expr.CaptureSet) => BinderInfo.Inaccessible(bd)
        val ctx1 = ctx.usingBinderInfos(binderInfos.reverse)
        val fields: List[(String, FieldType)] = classSym.info.fields.map: field =>
          val Expr.FieldInfo(fieldName, fieldType, fieldMut) = field
          val fieldSym = Symbol.fresh(fieldName)
          val tpe = translateType(fieldType)(using ctx1)
          val info = FieldType(fieldSym, tpe, mutable = fieldMut)
          (fieldName, info)
        val structType = 
          classSym.info.enumSymbol match
            case None =>
              StructType(fields.map(_._2), subClassOf = None)
            case Some(_) =>
              val fieldTypes = FieldType(Symbol.Tag, ValType.I32, mutable = false) :: fields.map(_._2)
              StructType(fieldTypes, subClassOf = Some(Symbol.EnumClass))
        val typeDef = TypeDef(structSym, structType)
        val nameMap = Map.from(fields.map((n, f) => (n, f.sym)))
        emitType(typeDef)
        val tagNumber = classSym.info.enumSymbol.map(_ => Tag.fresh())  // Create tag number for enum variants
        val layoutInfo = createLayoutInfo(structType)
        val structInfo = StructInfo(structSym, nameMap, layoutInfo, tagNumber)
        specMap += (specSig -> structInfo)
        structInfo

  def isPolySymbol(sym: Expr.DefSymbol)(using Context): Boolean =
    ctx.defInfos(sym) match
      case DefInfo.PolyFuncDef(_, _, _) => true
      case _ => false

  def createModuleFunction(sym: Expr.DefSymbol)(using Context): DefInfo.FuncDef =
    ctx.defInfos(sym) match
      case i: DefInfo.FuncDef => i
      case DefInfo.LazyFuncDef(body) =>
        val funcSym = Symbol.fresh(sym.name)
        val workerSym = Symbol.fresh(s"worker_${sym.name}")
        emitElemDeclare(ExportKind.Func, workerSym)
        ctx.defInfos += (sym -> DefInfo.FuncDef(funcSym, workerSym))
        genModuleFunction(sym.tpe, funcSym, Some(workerSym), body.asInstanceOf[Expr.Term])
        DefInfo.FuncDef(funcSym, workerSym)
      case DefInfo.PolyFuncDef(_, _, _) =>
        assert(false)
      case _ => 
        assert(false, "this is absurd: expecting a function symbol, but got a global value")

  def createPolyModuleFunction(sym: Expr.DefSymbol, targs: List[Expr.Type | Expr.CaptureSet])(using Context): DefInfo.FuncDef =
    val DefInfo.PolyFuncDef(typeParams, body, specMap) = ctx.defInfos(sym): @unchecked
    val specSig = translateTypeArgs(targs)
    specMap.get(specSig) match
      case Some(info) => info
      case None =>
        val encodedName = nameEncode(s"${sym.name}$$${specSig.encodedName}")
        val funcSym = Symbol.fresh(encodedName)
        val workerSym = Symbol.fresh(s"worker_${encodedName}")
        emitElemDeclare(ExportKind.Func, workerSym)
        val funcDef: DefInfo.FuncDef = DefInfo.FuncDef(funcSym, workerSym)
        specMap += (specSig -> funcDef)
        val typeBinderInfos = (typeParams `zip` targs).map:
          case (bd, tpe: Expr.Type) => BinderInfo.Specialized(bd, translateType(tpe))
          case (bd, tpe: Expr.CaptureSet) => BinderInfo.Inaccessible(bd)
        genModuleFunction(sym.tpe, funcSym, Some(workerSym), body.asInstanceOf[Expr.Term], typeBinderInfos = Some(typeBinderInfos))
        funcDef

  def maybeAddReturnCall(body: List[Instruction])(using Context): List[Instruction] =
    def goInstrs(instrs: List[Instruction]): Option[List[Instruction]] =
      go(instrs.last) match
        case None => None
        case Some(instr) => Some(instrs.init :+ instr)
    def go(instr: Instruction): Option[Instruction] = instr match
      case Instruction.Call(sym) => 
        // Change to a return call
        Some(Instruction.ReturnCall(sym))
      case Instruction.CallRef(sym) =>
        // Change to a return call
        Some(Instruction.ReturnCallRef(sym))
      case Instruction.If(resType, thenBranch, elseBranch) =>
        (goInstrs(thenBranch), goInstrs(elseBranch)) match
          case (Some(thenBranch1), Some(elseBranch1)) =>
            Some(Instruction.If(resType, thenBranch1, elseBranch1))
          case (Some(thenBranch1), None) =>
            Some(Instruction.If(resType, thenBranch1, elseBranch))
          case (None, Some(elseBranch1)) =>
            Some(Instruction.If(resType, thenBranch, elseBranch1))
          case (None, None) =>
            None
      case _ => None
    goInstrs(body).getOrElse(body)

  def declareLocal(symName: String, tpe: ValType)(using Context): Symbol =
    val sym = Symbol.fresh(symName)
    emitLocal(sym, tpe)
    sym

  def genArena(lambdaBody: Term.TermLambda, resType: Expr.Type)(using Context): List[Instruction] =
    val resValType = translateType(resType)
    val savedArenaPointerSym = declareLocal("__saved_arena_pointer", ValType.I32)
    val savedShadowTablePointerSym = declareLocal("__saved_shadow_table_pointer", ValType.I32)
    val resultSym = declareLocal("__result", resValType)
    val Term.TermLambda(ps, body, _) = lambdaBody
    val handleBinder :: Nil = ps: @unchecked
    val saveArenaInstrs = List(
      Instruction.GlobalGet(Symbol.ArenaCurrent),
      Instruction.LocalSet(savedArenaPointerSym),
      Instruction.GlobalGet(Symbol.ShadowTableCurrent),
      Instruction.LocalSet(savedShadowTablePointerSym),
    )
    val bodyInstrs = genTerm(body)(using ctx.withErasedBinder(handleBinder))
    val saveResultInstrs = List(
      Instruction.LocalSet(resultSym),
    )
    val restoreArenaInstrs = List(
      Instruction.LocalGet(savedArenaPointerSym),
      Instruction.GlobalSet(Symbol.ArenaCurrent),
      Instruction.LocalGet(savedShadowTablePointerSym),
      Instruction.GlobalSet(Symbol.ShadowTableCurrent),
    )
    val returnResultInstrs = List(
      Instruction.LocalGet(resultSym),
    )
    saveArenaInstrs ++ bodyInstrs ++ saveResultInstrs ++ restoreArenaInstrs ++ returnResultInstrs

  /** Generates code for a module-level function.
   * @param funType type of the module function
   * @param funSymbol output symbol for the function
   * @param workerSymbol output symbol for the worker function that wraps the module function as a first-class value
   * @param expr source language term of the function
   * @param typeBinderInfos specialized type binder infos when generating type-polymorphic functions
   */
  def genModuleFunction(
    funType: Expr.Type,
    funSymbol: Symbol,
    workerSymbol: Option[Symbol],
    expr: Expr.Term,
    typeBinderInfos: Option[List[BinderInfo]] = None,
  )(using Context): Unit = //trace(s"genModuleFunction($funType, $funSymbol, $workerSymbol, $expr)"):
    val (ps: List[Expr.Binder], body: Expr.Term, isTypeLambda: Boolean) = expr match
      case Term.TermLambda(ps, body, _) => (ps, body, false)
      case Term.TypeLambda(ps, body) => (ps, body, true)
      case _ => assert(false, "absurd")
    val (funcType, workerType) =
      typeBinderInfos match
        case Some(infos) => (computePolyFuncType(funType, infos, isClosure = false), computePolyFuncType(funType, infos, isClosure = true))
        case None => (computeFuncType(funType, isClosure = false), computeFuncType(funType, isClosure = true))
    val paramSymbols = 
      if isTypeLambda then Nil
      else ps.map(p => Symbol.fresh(p.name))
    val funcParams = paramSymbols `zip` funcType.paramTypes
    val workerSelfSymbol = Symbol.fresh("self")
    val workerParams = (workerSelfSymbol -> ValType.AnyRef) :: funcParams
    val func =
      newLocalsScope:
        val binderInfos = 
          if isTypeLambda then 
            typeBinderInfos match
              case Some(infos) => infos
              case None =>
                ps.map: bd =>
                  BinderInfo.Abstract(bd)
          else 
            (ps `zip` paramSymbols).map: (bd, sym) =>
              BinderInfo.Sym(bd, sym)
        val ctx1 = ctx.withMoreBinderInfos(binderInfos)
        val bodyInstrs = genTerm(body)(using ctx1)
        val bodyInstrs1 = maybeAddReturnCall(bodyInstrs)
        Func(
          funSymbol,
          funcParams,
          funcType.resultType,
          locals = finalizeLocals,
          body = bodyInstrs1,
        )
    emitFunc(func)
    workerSymbol match
      case Some(workerSym) =>
        val workerFunc =
          val getParamsInstrs =
            workerParams.tail.map: (sym, _) =>
              Instruction.LocalGet(sym)
          val callFuncInstrs = List(Instruction.Call(funSymbol))
          Func(
            workerSym,
            workerParams,
            funcType.resultType,
            locals = Nil,
            body = getParamsInstrs ++ callFuncInstrs
          )
        emitFunc(workerFunc)
      case None =>

  def createExtensionInfo(extSym: Expr.ExtensionSymbol, targs: List[Expr.Type | Expr.CaptureSet])(using Context): ExtensionInfo =
    val specSig = translateTypeArgs(targs)
    val specMap = ctx.extensionInfos.getOrElseUpdate(extSym, mutable.Map.empty)
    specMap.get(specSig) match
      case Some(info) => info
      case None =>
        val extInfo = extSym.info
        val sigName = specSig.encodedName
        val binderInfos = (extInfo.typeParams `zip` targs).map:
          case (bd, tpe: Expr.Type) =>
            BinderInfo.Specialized(bd, translateType(tpe))
          case (bd, tpe: Expr.CaptureSet) =>
            BinderInfo.Inaccessible(bd)
        val ctx1 = ctx.usingBinderInfos(binderInfos.reverse)
        // Create symbols
        val methodBaseNames: List[String] = extInfo.methods.map: method =>
          nameEncode(s"${extSym.name}$$$sigName$$${method.name}")
        val methodInfos: List[MethodInfo] = (methodBaseNames `zip` extInfo.methods).map: (baseName, method) =>
          if method.body.isTypePolymorphic then
            val TypePolymorphism(typeParams, _) = method.body: @unchecked
            def generator(targs: List[Expr.Type | Expr.CaptureSet], sym: Symbol): Unit =
              val typeBinderInfos = (typeParams `zip` targs).map:
                case (bd, tpe: Expr.Type) => BinderInfo.Specialized(bd, translateType(tpe))
                case (bd, tpe: Expr.CaptureSet) => BinderInfo.Inaccessible(bd)
              genModuleFunction(method.tpe, sym, workerSymbol = None, expr = method.body, Some(typeBinderInfos))(using ctx1)
            def computeType(targs: List[Expr.Type | Expr.CaptureSet]): FuncType =
              val typeBinderInfos = (typeParams `zip` targs).map:
                case (bd, tpe: Expr.Type) => BinderInfo.Specialized(bd, translateType(tpe))
                case (bd, tpe: Expr.CaptureSet) => BinderInfo.Inaccessible(bd)
              computePolyFuncType(method.tpe, typeBinderInfos, isClosure = false)(using ctx1)
            MethodInfo.PolyMethod(baseName, computeType, generator, mutable.Map.empty)
          else
            val funcSym = Symbol.fresh(baseName)
            MethodInfo.SimpleMethod(funcSym, computeFuncType(method.tpe, isClosure = false)(using ctx1))
        val methodMap = Map.from:
          (extInfo.methods `zip` methodInfos).map: (m, i) =>
            (m.name -> i)
        // Put symbols into the context
        specMap += (specSig -> ExtensionInfo(methodMap))
        // Generate method definitions for simple methods
        (methodInfos `zip` extInfo.methods).foreach: (methodInfo, method) =>
          methodInfo match
            case MethodInfo.SimpleMethod(sym, _) =>
              genModuleFunction(method.tpe, sym, workerSymbol = None, expr = method.body)(using ctx1)
            case _: MethodInfo.PolyMethod =>
        specMap(specSig)

  def getPolyMethodInfo(extInfo: ExtensionInfo, name: String, targs: List[Expr.Type | Expr.CaptureSet])(using Context): MethodInfo.SimpleMethod =
    extInfo.methodMap(name) match
      case simp: MethodInfo.SimpleMethod => simp
      case poly: MethodInfo.PolyMethod =>
        val specSig = translateTypeArgs(targs)
        poly.specMap.get(specSig) match
          case Some(info) => info
          case None =>
            val funcName = nameEncode(s"${poly.baseName}$$${specSig.encodedName}")
            val funcSym = Symbol.fresh(funcName)
            val funcType = poly.computeType(targs)
            val simpleInfo: MethodInfo.SimpleMethod = MethodInfo.SimpleMethod(funcSym, funcType)
            poly.specMap += (specSig -> simpleInfo)
            poly.generator(targs, funcSym)
            simpleInfo

  def isValidMain(d: Expr.Definition)(using Context): Boolean = d match
    case Definition.ValDef(sym, body) if sym.name == "main" =>
      val mainType = Expr.Type.TermArrow(Nil, Expr.Type.Base(Expr.BaseType.I32))
      val mainFuncType = computeFuncType(mainType)
      val defType = sym.tpe
      computeFuncType(defType) == mainFuncType
    case _ => false

  def toNullable(tp: ValType): ValType = tp match
    case ValType.TypedRef(sym, _) => ValType.TypedRef(sym, nullable = true)
    case _ => tp

  def makeDefaultValue(tp: ValType): Instruction = tp match
    case ValType.I32 => Instruction.I32Const(0)
    case ValType.I64 => Instruction.I64Const(0)
    case ValType.AnyRef => Instruction.RefNullAny
    case ValType.TypedRef(sym, nullable) if nullable => Instruction.RefNull(sym)
    case _ => assert(false, s"Unsupported type for making default value: $tp")

  def emitDefaultImports()(using Context): Unit =
    emitImportFunc(ImportFunc("host", "println_i32", Symbol.I32Println, I32PrintlnType))
    emitImportFunc(ImportFunc("host", "read_i32", Symbol.I32Read, I32ReadType))
    emitImportFunc(ImportFunc("host", "println_char", Symbol.PutChar, PutCharType))
    emitImportFunc(ImportFunc("host", "get_timestamp", Symbol.PerfCounter, FuncType(Nil, Some(ValType.I32))))

  def emitDefaultMemory()(using Context): Unit =
    val memory = Memory(Symbol.Memory, 1)
    emitMemory(memory)

  def setupArena()(using Context): Unit =
    val arenaMemory = Memory(Symbol.ArenaMemory, 64)
    emitMemory(arenaMemory)
    val arenaPointer = Global(Symbol.ArenaCurrent, ValType.I32, mutable = true, Instruction.I32Const(0))
    emitGlobal(arenaPointer)
    val shadowTable = Table(Symbol.ShadowTable, 65536, ValType.AnyRef)
    emitTable(shadowTable)
    val shadowTableCurrent = Global(Symbol.ShadowTableCurrent, ValType.I32, mutable = true, Instruction.I32Const(0))
    emitGlobal(shadowTableCurrent)

  def emitDefaultTypes()(using Context): Unit =
    val td = TypeDef(
      Symbol.EnumClass, 
      StructType(List(FieldType(Symbol.Tag, ValType.I32, mutable = false)), subClassOf = None)
    )
    emitType(td)

  def locateMainFunction(ms: List[Expr.Module])(using Context): Option[Expr.DefSymbol] =
    ms.flatMap(_.defns).find(isValidMain) match
      case Some(Definition.ValDef(sym, _)) => Some(sym)
      case Some(_) => assert(false, "absurd")
      case None => None

  def genModules(ms: List[Expr.Module])(using Context): Unit =
    val allDefns = ms.flatMap(_.defns)
    val mainSym = locateMainFunction(ms).getOrElse(assert(false, "No valid main function in module"))
    // First of all, emit imports and default memory
    emitDefaultTypes()
    emitDefaultImports()
    emitDefaultMemory()
    // Also, setup the arena
    setupArena()
    // Next, create symbols for all the definitions
    //   for struct symbols, we create the type as well
    //   for function symbols, we create placeholders, either lazy or polymorphic
    allDefns.foreach: defn =>
      defn match
        case Definition.ValDef(sym, body) =>
          body match
            case body: Expr.Closure =>
              // Do nothing for now, we emit the function lazily
              body match
                case TypePolymorphism(typeParams, _) =>
                  ctx.defInfos += (sym -> DefInfo.PolyFuncDef(typeParams, body, mutable.Map.empty))
                case _ =>
                  ctx.defInfos += (sym -> DefInfo.LazyFuncDef(body))
            case _ =>
              val globalSym = Symbol.fresh(sym.name)
              ctx.defInfos += (sym -> DefInfo.GlobalDef(globalSym))
        case Definition.StructDef(sym) =>
          ctx.typeInfos += (sym -> mutable.Map.empty)
        case Definition.EnumDef(sym) =>
          sym.info.variants.foreach: vSym =>
            ctx.typeInfos += (vSym -> mutable.Map.empty)
        case Definition.ExtensionDef(sym) =>
          ctx.extensionInfos += (sym -> mutable.Map.empty)
        case Definition.TypeDef(sym) =>
          // Type definitions do not have runtime representation
    // Next, emit global definitions
    allDefns.foreach: defn =>
      defn match
        case Definition.ValDef(sym, body) =>
          body match
            case body: Term.TermLambda =>
            case body: Term.TypeLambda =>
            case body =>
              val valType = toNullable(translateType(sym.tpe))
              val defaultVal = makeDefaultValue(valType)
              val DefInfo.GlobalDef(globalSym) = ctx.defInfos(sym): @unchecked
              val g = Global(globalSym, valType, mutable = true, defaultVal)
              emitGlobal(g)
        case _ =>
    // Next, emit the start function
    val startFuncSymbol = Symbol.fresh("init")
    val startFunc =
      newLocalsScope:
        val instrs =
          allDefns.flatMap: defn =>
            defn match
              case Definition.ValDef(sym, body) =>
                body match
                  case body: Term.TermLambda => Nil
                  case body: Term.TypeLambda => Nil
                  case body =>
                    val valType = toNullable(translateType(sym.tpe))
                    val DefInfo.GlobalDef(globalSym) = ctx.defInfos(sym): @unchecked
                    val bodyInstrs = genTerm(body)
                    val setGlobalInstrs = List(
                      Instruction.GlobalSet(globalSym)
                    )
                    bodyInstrs ++ setGlobalInstrs
              case _ => Nil
        Func(
          startFuncSymbol,
          List(),
          None,
          finalizeLocals,
          instrs
        )
    ctx.startFunc = Some(startFuncSymbol)
    emitFunc(startFunc)
    // Lastly, emit the main function
    val DefInfo.FuncDef(mainFunc, _) = createModuleFunction(mainSym)
    val exp = Export("entrypoint", ExportKind.Func, mainFunc)
    emitExport(exp)

  def finalize(using Context): Module =
    Module(
      ctx.declares.toList ++ 
        ctx.imports.toList ++ 
        ctx.memories.toList ++
        ctx.tables.toList ++
        ctx.types.toList ++ 
        ctx.globals.toList ++
        ctx.funcs.toList ++ 
        ctx.exports.toList ++
        ctx.startFunc.map(Start(_)).toList
    )
