package poly

import poly.MyList.*

type Stack = MyList[Int]

enum Instruction:
  case Push(v: Int)
  case Pop
  case Add
  case Sub
  case Mul
  case Div

import Instruction.*

type Program = MyList[Instruction]

final class NotEnoughOperandsInStackException extends Exception(f"Not enough operands in stack.")
final class DivideByZeroException extends Exception("Divide by 0.")

object Interpreter:

  // start interpret-inst
  def interpreteInst(stack: Stack, inst: Instruction): Stack = ???
  // end interpret-inst

  // start interpret-prog
  def interpreteProg(stack: Stack, program: Program): Stack = ???
  // end interpret-prog
