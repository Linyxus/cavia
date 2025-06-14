package cavia.core.ast

object Wasm:
  case class Module(fields: List[ModuleField])

  // Marker trait for reference types.
  sealed trait ReferenceType:
    def getTypeSymbol: Option[Symbol] = this match
      case ValType.TypedRef(sym, nullable) => Some(sym)
      case ValType.AnyRef => None

  enum ValType:
    case I32
    case I64
    case F64
    case TypedRef(sym: Symbol, nullable: Boolean = false) extends ValType, ReferenceType
    case AnyRef extends ValType, ReferenceType

    def show: String = this match
      case ValType.I32 => "i32"
      case ValType.I64 => "i64"
      case ValType.F64 => "f64"
      case ValType.TypedRef(sym, nullable) => 
        val nullText = if nullable then "null " else ""
        s"(ref $nullText${sym.show})"
      case ValType.AnyRef => "anyref"

  enum Instruction:
    case I32Const(value: Int)
    case I32Add
    case I32Sub
    case I32Mul
    case I32Div
    case I32Rem
    case I32Gte
    case I32Lte
    case I32Gt
    case I32Lt
    case I32Eq
    case I32Ne
    case I32Eqz
    case I64Const(value: Int)
    case I64Add
    case I64Sub
    case I64Mul
    case I64Div
    case I64Rem
    case I64Gte
    case I64Lte
    case I64Gt
    case I64Lt
    case I64Eq
    case I64Ne
    case I64Eqz
    case F64Const(value: Double)
    case F64Add
    case F64Sub
    case F64Mul
    case F64Div
    case F64Gte
    case F64Lte
    case F64Gt
    case F64Lt
    case F64Eq
    case F64Ne
    case LocalSet(sym: Symbol)
    case LocalGet(sym: Symbol)
    case LocalTee(sym: Symbol)
    case GlobalSet(sym: Symbol)
    case GlobalGet(sym: Symbol)
    case RefCast(typeSym: Symbol)
    case RefTest(typeSym: Symbol)
    case RefFunc(funcSym: Symbol)
    case RefNull(typeSym: Symbol)
    case RefNullAny
    case StructGet(sym: Symbol, fieldSym: Symbol)
    case StructSet(sym: Symbol, fieldSym: Symbol)
    case StructNew(typeSym: Symbol)
    case CallRef(typeSym: Symbol)
    case Call(funcSym: Symbol)
    case ReturnCall(funcSym: Symbol)
    case ReturnCallRef(typeSym: Symbol)
    case If(resultType: ValType, thenBranch: List[Instruction], elseBranch: List[Instruction])
    case ArrayNew(typeSym: Symbol)
    case ArrayNewFixed(typeSym: Symbol, size: Int)
    case ArraySet(typeSym: Symbol)
    case ArrayGet(typeSym: Symbol)
    case ArrayLen
    case Load(tpe: ValType, memorySym: Symbol)
    case Store(tpe: ValType, memorySym: Symbol)
    case MemorySize(memorySym: Symbol)
    case TableGet(tableSym: Symbol)
    case TableSet(tableSym: Symbol)
    case TableSize(tableSym: Symbol)
    case TableGrow(tableSym: Symbol)
    case TableFill(tableSym: Symbol)
    case Drop
    case Unreachable

    def showIfSimple: Option[String] = this match
      case I32Const(value) => Some(s"i32.const $value")
      case I32Add => Some("i32.add")
      case I32Sub => Some("i32.sub")
      case I32Mul => Some("i32.mul")
      case I32Div => Some("i32.div_s")
      case I32Rem => Some("i32.rem_s")
      case I32Gte => Some("i32.ge_s")
      case I32Lte => Some("i32.le_s")
      case I32Gt => Some("i32.gt_s")
      case I32Lt => Some("i32.lt_s")
      case I32Eq => Some("i32.eq")
      case I32Ne => Some("i32.ne")
      case I32Eqz => Some("i32.eqz")
      case I64Const(value) => Some(s"i64.const $value")
      case I64Add => Some("i64.add")
      case I64Sub => Some("i64.sub")
      case I64Mul => Some("i64.mul")
      case I64Div => Some("i64.div_s")
      case I64Rem => Some("i64.rem_s")
      case I64Gte => Some("i64.ge_s")
      case I64Lte => Some("i64.le_s")
      case I64Gt => Some("i64.gt_s")
      case I64Lt => Some("i64.lt_s")
      case I64Eq => Some("i64.eq")
      case I64Ne => Some("i64.ne")
      case I64Eqz => Some("i64.eqz")
      case F64Const(value) => Some(s"f64.const $value")
      case F64Add => Some("f64.add")
      case F64Sub => Some("f64.sub")
      case F64Mul => Some("f64.mul")
      case F64Div => Some("f64.div")
      case F64Gte => Some("f64.ge")
      case F64Lte => Some("f64.le")
      case F64Gt => Some("f64.gt")
      case F64Lt => Some("f64.lt")
      case F64Eq => Some("f64.eq")
      case F64Ne => Some("f64.ne")
      case LocalSet(sym) => Some(s"local.set ${sym.show}")
      case LocalGet(sym) => Some(s"local.get ${sym.show}")
      case LocalTee(sym) => Some(s"local.tee ${sym.show}")
      case GlobalSet(sym) => Some(s"global.set ${sym.show}")
      case GlobalGet(sym) => Some(s"global.get ${sym.show}")
      case RefCast(typeSym) => Some(s"ref.cast (ref ${typeSym.show})")
      case RefTest(typeSym) => Some(s"ref.test (ref ${typeSym.show})")
      case RefFunc(funcSym) => Some(s"ref.func ${funcSym.show}")
      case StructGet(sym, fieldSym) => Some(s"struct.get ${sym.show} ${fieldSym.show}")
      case StructSet(sym, fieldSym) => Some(s"struct.set ${sym.show} ${fieldSym.show}")
      case StructNew(typeSym) => Some(s"struct.new ${typeSym.show}")
      case CallRef(typeSym) => Some(s"call_ref ${typeSym.show}")
      case Call(funcSym) => Some(s"call ${funcSym.show}")
      case ReturnCall(funcSym) => Some(s"return_call ${funcSym.show}")
      case RefNull(typeSym) => Some(s"ref.null ${typeSym.show}")
      case RefNullAny => Some("ref.null any")
      case ReturnCallRef(typeSym) => Some(s"return_call_ref ${typeSym.show}")
      case ArrayNew(typeSym) => Some(s"array.new ${typeSym.show}")
      case ArrayNewFixed(typeSym, size) => Some(s"array.new_fixed ${typeSym.show} $size")
      case ArraySet(typeSym) => Some(s"array.set ${typeSym.show}")
      case ArrayGet(typeSym) => Some(s"array.get ${typeSym.show}")
      case ArrayLen => Some(s"array.len")
      case TableGet(tableSym) => Some(s"table.get ${tableSym.show}")
      case TableSet(tableSym) => Some(s"table.set ${tableSym.show}")
      case TableSize(tableSym) => Some(s"table.size ${tableSym.show}")
      case TableGrow(tableSym) => Some(s"table.grow ${tableSym.show}")
      case TableFill(tableSym) => Some(s"table.fill ${tableSym.show}")
      case Load(tpe, memorySym) => 
        Some(s"${tpe.show}.load ${memorySym.show}")
      case Store(tpe, memorySym) => 
        Some(s"${tpe.show}.store ${memorySym.show}")
      case MemorySize(memorySym) =>
        Some(s"memory.size ${memorySym.show}")
      case Drop => Some("drop")
      case Unreachable => Some("unreachable")
      case _ => None

  enum ExportKind:
    case Func
    case Table
    case Memory

    def show: String = this match
      case ExportKind.Func => "func"
      case ExportKind.Table => "table"
      case ExportKind.Memory => "memory"

  sealed trait Symbol:
    def show: String
  case class UniqSymbol(val name: String, val uniqId: Int) extends Symbol:
    def show: String = s"$$${name}@${uniqId}"

    override def hashCode(): Int = name.hashCode() + uniqId + 1

  object NoSymbol extends Symbol:
    def show: String = assert(false, "No symbol")
    override def hashCode(): Int = 0

  object Symbol:
    private var nextId = 0
    def fresh(name: String): Symbol =
      val id = nextId
      nextId += 1
      UniqSymbol(name, id)

    val Function = fresh("__func")
    val I32Println = fresh("__i32println")
    val I32Read = fresh("__i32read")
    val Memory = fresh("__memory")
    val ArenaMemory = fresh("__arena_memory")
    val ArenaCurrent = fresh("__arena_current")
    val ShadowTable = fresh("__shadow_table")
    val ShadowTableCurrent = fresh("__shadow_table_current")
    val PutChar = fresh("__putchar")
    val PerfCounter = fresh("__perf_counter")
    val EnumClass = fresh("__enum")
    val Tag = fresh("__tag")
  
  object Tag:
    private var nextId = 0
    def fresh(): Int =
      val id = nextId
      nextId += 1
      id

  val I32PrintlnType = FuncType(List(ValType.I32), None)
  val I32ReadType = FuncType(List(), Some(ValType.I32))
  val PutCharType = FuncType(List(ValType.I32), None)
  case class FieldType(sym: Symbol, tpe: ValType, mutable: Boolean)

  sealed trait CompositeType:
    def show: String

  case class FuncType(paramTypes: List[ValType], resultType: Option[ValType]) extends CompositeType:
    def show: String =
      val paramStrs = paramTypes.map(p => s"(param ${p.show})")
      val resultStr = 
        resultType match
          case None => ""
          case Some(resultType) => s"(result ${resultType.show})"
      s"(func ${paramStrs.mkString(" ")} ${resultStr})"
  case class StructType(fields: List[FieldType], subClassOf: Option[Symbol]) extends CompositeType:
    def show: String = 
      val fieldStrs = fields.map: 
        case FieldType(sym, tpe, mutable) =>
          val typeStr = if mutable then s"(mut ${tpe.show})" else tpe.show
          s"(field ${sym.show} $typeStr)"
      val subclassStr = subClassOf match
        case None => ""
        case Some(sym) => s"${sym.show} "
      s"(sub ${subclassStr}(struct ${fieldStrs.mkString(" ")}))"
  case class ArrayType(elemType: ValType, mutable: Boolean) extends CompositeType:
    def show: String = 
      val typeStr = if mutable then s"(mut ${elemType.show})" else elemType.show
      s"(array $typeStr)"

  sealed trait ModuleField
  case class Func(ident: Symbol, params: List[(Symbol, ValType)], result: Option[ValType], locals: List[(Symbol, ValType)], body: List[Instruction]) extends ModuleField:
    def tpe: FuncType = FuncType(params.map(_._2), result)
  case class Export(externalName: String, kind: ExportKind, ident: Symbol) extends ModuleField
  case class TypeDef(ident: Symbol, tpe: CompositeType) extends ModuleField
  case class ElemDeclare(kind: ExportKind, sym: Symbol) extends ModuleField
  case class ImportFunc(moduleName: String, funcName: String, ident: Symbol, funcType: FuncType) extends ModuleField
  case class Global(ident: Symbol, tpe: ValType, mutable: Boolean, init: Instruction) extends ModuleField
  case class Start(funcSym: Symbol) extends ModuleField
  case class Memory(ident: Symbol, size: Int) extends ModuleField
  case class Table(ident: Symbol, size: Int, elemType: ValType) extends ModuleField

  class CodeBuilder:
    import collection.mutable.ArrayBuffer
    private var code: ArrayBuffer[Instruction] = ArrayBuffer.empty
    def emit(instr: Instruction*): Unit =
      code ++= instr.toList
    def emit(instrs: List[Instruction]): Unit =
      code ++= instrs
    def output: List[Instruction] = code.toList
