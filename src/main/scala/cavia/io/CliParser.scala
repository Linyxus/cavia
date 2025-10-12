package cavia
package io

import util.boundary, boundary.break

object CliParser:
  enum Outcome:
    case Ok
    case Err(msg: String)

    def isOk: Boolean = this match
      case Ok => true
      case Err(_) => false

    def isErr: Boolean = !isOk
  import Outcome.*

  abstract class CliOption[A](initValue: A):
    protected var _value: A = initValue
    val name: String
    def receive(arg: String): Option[Outcome]
    def get: A = _value

  class SourceFilesOption extends CliOption[List[SourceFile]](Nil):
    val name = "source-files"
    def receive(arg: String): Option[Outcome] =
      if arg.endsWith(".scala") || arg.endsWith(".cavia") then
        this._value ++= List(SourceFile.fromPath(arg))
        Some(Ok)
      else None

  def convertToBoolean(argVal: String): Option[Boolean] =
    argVal match
      case "true" => Some(true)
      case "false" => Some(false)
      case "0" => Some(false)
      case "1" => Some(true)
      case "yes" => Some(true)
      case "no" => Some(false)
      case _ => None

  class TrueOrFalseOption(val name: String) extends CliOption[Boolean](false):
    def receive(arg: String): Option[Outcome] =
      if arg.contains(":=") then
        val (key, value) = arg.splitAt(arg.indexOf(":="))
        if key == name then
          convertToBoolean(value) match
            case None => Some(Err(s"Expecting a boolean value, got $value"))
            case Some(b) =>
              this._value = b
              Some(Ok)
        else None
      else if arg == s"+$name" then
        this._value = true
        Some(Ok)
      else if arg == s"-$name" then
        this._value = false
        Some(Ok)
      else None

  abstract class GenericCliParser:
    def options: List[CliOption[?]]

    def parseAll(args: List[String]): Outcome = boundary:
      for arg <- args do
        parseThisArg(arg) match
          case Ok =>
          case Err(msg) =>
            break(Err(msg))
      break(Ok)

    def parseThisArg(arg: String): Outcome = boundary:
      for option <- options do
        option.receive(arg) match
          case Some(outcome) => break(outcome)
          case None =>
      Err(s"Invalid argument: $arg. None of the options matched.")

class CompilerCliParser extends CliParser.GenericCliParser:
  import CliParser.*

  lazy val myOptions: List[CliOption[?]] = List(
    printIdsOption,
    printParserOption,
    includeStdOption,
    sourceFilesOption,
  )

  val printIdsOption = new TrueOrFalseOption("printids")
  val includeStdOption = new TrueOrFalseOption("std")
  val sourceFilesOption = new SourceFilesOption()
  val printParserOption = new TrueOrFalseOption("printparser")

  def options: List[CliOption[?]] = myOptions
