package cavia
package core

import io.*

object CompilerSettings:
  enum CompilerAction:
    case Check(sourceFiles: List[SourceFile])
    case Codegen(sourceFiles: List[SourceFile])
    case Help
