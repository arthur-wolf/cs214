package patmat

object TriBoolOps:
  import TriBool.*

  def neg(b: TriBool): TriBool =
    b match
      case Maybe => Maybe
      case Yes => No
      case No => Yes

  def and(b1: TriBool, b2: TriBool): TriBool = 
    (b1, b2) match
      case (Yes, Yes) => Yes
      case (No, _) => No
      case (_, No) => No
      case (_, _) => Maybe
    
  def or(b1: TriBool, b2: TriBool): TriBool =
    (b1, b2) match
      case (No, No) => No
      case (Yes, _) => Yes
      case (_, Yes) => Yes
      case (_, _) => Maybe
    

  def nand(b1: TriBool, b2: TriBool): TriBool =
    neg(and(b1,b2))


