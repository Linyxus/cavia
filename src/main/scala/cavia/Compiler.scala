package cavia

import io.*
import core.ast.*
import core.*
import CompilerSettings.*
import tokenizing.*
import parsing.*
import reporting.*
import Printer.*
import typechecking.*
import codegen.*
import rechecking.*
import java.nio.file.*
import core.ast.expr.Expr

object Compiler:
  class ParseStep(config: CompilerConfig) extends CompilerStep[List[SourceFile], List[Syntax.Module]]:
    def run(sources: List[SourceFile]): Outcome[List[Syntax.Module]] =
      parseAll(sources) match
        case Left(err: Tokenizer.Error) => Outcome.simpleFailure(err.asMessage)
        case Left(err: Parser.ParseError) => Outcome.simpleFailure(Message.multiline(getErrorMessages(err), err.pos))
        case Right(modules) => 
          if config.printParser then
            println("--- parsed modules")
            modules.foreach: m =>
              if m.sourceFile.name != "<stdlib>" then
                println(m)
          Outcome.simpleSuccess(modules)

  class TypecheckStep(config: CompilerConfig) extends CompilerStep[List[Syntax.Module], List[Expr.Module]]:
    def run(sources: List[Syntax.Module]): Outcome[List[Expr.Module]] =
      val ctx = TypeChecker.Context.empty.copy(config = config)
      TypeChecker.checkModules(sources)(using ctx) match
        case Left(errs) =>
          new Outcome:
            def getOption = None
            def getMessages = errs.map(_.asMessage)
        case Right(res) => 
          val checker = new ArenaSetup
          res.foreach: m =>
            if m.name != "std" then
              checker.recheckModule(m)(using ctx)
          Outcome.simpleSuccess(res)

  class PrintModulesStep(after: String, config: CompilerConfig) extends CompilerStep[List[Expr.Module], List[Expr.Module]]:
    def run(modules: List[Expr.Module]): Outcome[List[Expr.Module]] =
      println(s"--- modules after $after")
      modules.foreach: m =>
        if m.name != "std" then
          println(ExprPrinter.show(m)(using TypeChecker.Context.emptyWithConfig(config)))
      Outcome.simpleSuccess(modules)

  class CodegenStep(outputPath: String) extends CompilerStep[List[Expr.Module], Unit]:
    def run(modules: List[Expr.Module]): Outcome[Unit] =
      given genCtx: CodeGenerator.Context = CodeGenerator.Context()
      CodeGenerator.genModules(modules)
      val wasmMod = CodeGenerator.finalize
      val outputCode = wasmMod.show
      // println("--- wasm module")
      // println(outputCode)
      Files.writeString(Path.of(outputPath), outputCode)
      Outcome.simpleSuccess(())

  class CompileStep(inputPath: String, outputPath: String) extends CompilerStep[Unit, Unit]:
    def run(x: Unit): Outcome[Unit] = 
      val res = WasmAssembler.compile(inputPath, outputPath)
      res match
        case ShellUtils.ShellResult.Ok(msg) =>
          println(s"--- compiled to $outputPath")
          WasmOptimizer.optimize(outputPath, outputPath) match
            case ShellUtils.ShellResult.Ok(msg) =>
              println(s"--- optimized to $outputPath")
              Outcome.simpleSuccess(())
            case ShellUtils.ShellResult.Err(msg, code) =>
              Outcome.simpleFailure(Message.simple(msg, null))
        case ShellUtils.ShellResult.Err(msg, code) =>
          Outcome.simpleFailure(Message.simple(msg, null))

  def parseConfig(args: List[String]): Either[String, CompilerCliParser] =
    val p = new CompilerCliParser()
    p.parseAll(args) match
      case CliParser.Outcome.Ok => Right(p)
      case CliParser.Outcome.Err(msg) => Left(msg)

  def parseOptions(args: List[String]): Either[String, CompilerAction] =
    args match
      case Nil => Right(CompilerAction.Help)
      case "check" :: moreArgs => hopefully:
        val p = parseConfig(moreArgs).!!
        val config = CompilerConfig(
          printIds = p.printIdsOption.get,
          includeStd = p.includeStdOption.get,
          printParser = p.printParserOption.get,
        )
        CompilerAction.Check(p.sourceFilesOption.get, config)
      case "gen" :: moreArgs => hopefully:
        val p = parseConfig(moreArgs).!!
        val config = CompilerConfig(
          printIds = p.printIdsOption.get,
          includeStd = p.includeStdOption.get,
          printParser = p.printParserOption.get,
        )
        CompilerAction.Codegen(p.sourceFilesOption.get, config)
      case "compile" :: moreArgs => hopefully:
        val p = parseConfig(moreArgs).!!
        val config = CompilerConfig(
          printIds = p.printIdsOption.get,
          includeStd = p.includeStdOption.get,
          printParser = p.printParserOption.get,
        )
        CompilerAction.Compile(p.sourceFilesOption.get, config)
      case  cmd :: _ => Left(s"Invalid command: $cmd")

  def run(action: CompilerAction): Unit =
    action match
      case CompilerAction.Help =>
        println("Usage: caviac check <source files>")
        println("       caviac gen <source files>")
        println("       caviac compile <source files>")
      case CompilerAction.Check(sources, config) =>
        val sources1 = sources :+ stdlib
        val runner = ParseStep(config) `fuse` TypecheckStep(config) `fuse` PrintModulesStep("typecheck", config)
        val res = runner.execute(sources1)
        res match
          case Some(modules) =>
          case None =>
      case CompilerAction.Codegen(sources, config) =>
        val runner = 
          ParseStep(config)
            `fuse` TypecheckStep(config)
            `fuse` PrintModulesStep("typecheck", config)
            `fuse` CodegenStep("out.wat")
        val res = runner.execute(sources :+ stdlib)
        res match
          case Some(_) =>
            println("--- codegen successful")
          case None =>
            println("--- codegen failed")
      case CompilerAction.Compile(sources, config) =>
        val runner = 
          ParseStep(config)
            `fuse` TypecheckStep(config)
            `fuse` PrintModulesStep("typecheck", config)
            `fuse` CodegenStep("out.wat")
            `fuse` CompileStep("out.wat", "out.wasm")
        val res = runner.execute(sources :+ stdlib)
        if res.isDefined then
          println("--- compilation succeeded")
        else
          println("--- compilation failed")

  type ParseResult[+X] = Either[Tokenizer.Error | Parser.ParseError, X]

  def parseAll(sources: List[SourceFile]): ParseResult[List[Syntax.Module]] =
    hopefully:
      val modules = sources.map(parse(_).!!)
      (sources `zip` modules).foreach: (source, module) =>
        module.sourceFile = source
      modules

  def parse(source: SourceFile): ParseResult[Syntax.Module] =
    val tokens = Tokenizer.tokenize(source)
    val errTokens = tokens.collect { case err: Tokenizer.Error => err }
    if errTokens.nonEmpty then
      Left(errTokens.head)
    else
      // println("Tokenization successful")
      val tokenArray = (tokens.collect { case token: Token => token }).toArray
      // tokenArray.foreach: token =>
      //   println(Printer.showSourcePos(token.pos, List(token.toString)))
      val state = Parser.ParserState(tokenArray, 0)
      val ctx = Parser.ParserContext.empty
      Parsers.moduleP.runParser(state)(using ctx) match
        case Parser.ParseResult(nextState, Left(err)) =>
          val errs = ctx.errors.sortBy(_.pos.span.end)
          // println("Errors:")
          // errs.foreach: err =>
          //   println(Printer.showSourcePos(err.pos, List(err.toString)))
          val farestErr = if errs.nonEmpty then errs.last else err
          Left(farestErr)
        case Parser.ParseResult(nextState, Right(result)) =>
          Right(result)

  def compile(source: SourceFile): Unit =
    val tokens = Tokenizer.tokenize(source)
    val errTokens = tokens.collect { case err: Tokenizer.Error => err }
    if errTokens.nonEmpty then
      println("Tokenization errors:")
      errTokens.foreach: err =>
        println(Printer.showSourcePos(err.pos, List(err.toString)))
    else
      val tokenArray = (tokens.collect { case token: Token => token }).toArray
      println("Tokenization successful")
      tokenArray.foreach: token =>
        println(Printer.showSourcePos(token.pos, List(token.toString)))
      val state = Parser.ParserState(tokenArray, 0)
      val ctx = Parser.ParserContext.empty
      Parsers.moduleP.runParser(state)(using ctx) match
        case Parser.ParseResult(nextState, Left(err)) =>
          println(err.show)
        case Parser.ParseResult(nextState, Right(result)) =>
          println("Parsing successful")
          println(Printer.showSourcePos(result.pos, List(result.toString)))

//   val stdlib: SourceFile = SourceFile(
//     name = "<stdlib>",
//     content = """
// module std
// """)

  val stdlib: SourceFile = SourceFile(
    name = "<stdlib>",
    content = """
module std
// Counter
struct Counter(var count: i32)
extension (c: Counter^)
  def inc(): Unit =
    c.count = c.get() + 1
  def get(): i32 = c.count
// Range
struct Range(start: i32, end: i32)
extension (i: i32)
  def until(end: i32): Range^ = Range(i, end)
extension (r: Range^)
  def iterate(op: i32 => Unit): Unit =
    val cnt = Counter(r.start)
    def loop(): Unit =
      if cnt.get() < r.end then
        op(cnt.get())
        cnt.inc()
        loop()
    loop()
// Array
extension [T](xs: array[T]^)
  def mapInPlace(f: T => T): Unit =
    def loop(i: i32): Unit =
      if i < xs.length then
        xs(i) = f(xs(i))
        loop(i+1)
    loop(0)
  def foreach(f: T => Unit): Unit =
    def op(x: T): T =
      f(x)
      x
    xs.mapInPlace(op)
// String and IO
type String = array[char]
def putChar(ch: char): Unit = #putchar(ch)
def putStr(s: String^): Unit =
  s.foreach(putChar)
def putStrLn(s: String^): Unit =
  putStr(s)
  putChar('\n')
extension (self: String^)
  def concat(other: String^): String^ =
    val res = newArray[char](self.size + other.size, ' ')
    (0.until(self.size)).iterate((i: i32) => res(i) = self(i))
    (0.until(other.size)).iterate((i: i32) => res(i + self.size) = other(i))
    res
// Performance counter
def perfCounter(): i32 = #perfcounter()
def benchmark(body: () => Unit): i32 =
  val start = perfCounter()
  body()
  val end = perfCounter()
  end - start
// List
enum List[+T]:
  case Nil()
  case Cons(head: T, tail: List[T]^)
extension [T](xs: List[T]^)
  def map[U](f: T => U): List[U]^ =
    xs match
      case Nil() => Nil[U]()
      case Cons(x, xs) =>
        Cons(f(x), xs.map[U](f))
  def foreach(f: T => Unit): Unit =
    xs.map[Unit]: (x: T) =>
      f(x)
  def filter(f: T => bool): List[T]^ =
    xs match
      case Nil() => Nil[T]()
      case Cons(x, xs) =>
        if f(x) then Cons(x, xs.filter(f))
        else xs.filter(f)
  def reverse: List[T]^ =
    def recur(xs: List[T]^, consume acc: List[T]^): List[T]^ =
      xs match
        case Nil() => acc
        case Cons(x, xs) =>
          recur(xs, Cons(x, acc))
    recur(xs, Nil[T]())
  def concat(other: List[T]^): List[T]^{other, cap} =
    def recur(todos: List[T]^, acc: List[T]^): List[T]^{acc, cap} =
      todos match
        case Nil() => acc
        case Cons(todo, todos) =>
          recur(todos, Cons(todo, acc))
    val t1 = xs.reverse
    recur(xs.reverse, other)
  def length: i32 =
    xs match
      case Nil() => 0
      case Cons(_, xs) => 1 + xs.length
  def at(idx: i32): T =
    xs match
      case Cons(x, xs) =>
        if idx <= 0 then x
        else xs.at(idx-1)
""")
