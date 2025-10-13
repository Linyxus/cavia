package cavia

import io.*

object CompilerSettings:
  enum CompilerAction:
    /** Typecheck the source files. */
    case Check(sourceFiles: List[SourceFile], conifg: CompilerConfig)
    /** Typecheck source files and generate .wat code. */
    case Codegen(sourceFiles: List[SourceFile], config: CompilerConfig)
    /** Typecheck, generate .wat code, then compile to an optimized .wasm binary. */
    case Compile(sourceFiles: List[SourceFile], config: CompilerConfig)
    /** Start the repl */
    case Repl()
    /** Print the help message. */
    case Help

  case class CompilerConfig(
    /** Whether to print the ids of `cap`-instances. */
    printIds: Boolean,
    /** Whether to include standard library. */
    includeStd: Boolean,
    /** Whether to print parsed trees. */
    printParser: Boolean,
  ):
    def turnOffPrintIds: CompilerConfig = copy(printIds = false)

  object CompilerConfig:
    val default: CompilerConfig = CompilerConfig(
      printIds = true,
      includeStd = true,
      printParser = false,
    )
