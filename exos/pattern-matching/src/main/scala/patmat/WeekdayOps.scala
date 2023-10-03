package patmat

import Weekday.*

object WeekdayOps:

  def next(d: Weekday): Weekday =
    d match
      case Monday => Tuesday
      case Tuesday => Wednesday
      case Wednesday => Thursday
      case Thursday => Friday
      case Friday => Saturday
      case Saturday => Sunday
      case Sunday => Monday
    

  def prev(d: Weekday): Weekday =
    d match
      case Monday => Sunday
      case Tuesday => Monday
      case Wednesday => Tuesday
      case Thursday => Wednesday
      case Friday => Thursday
      case Saturday => Friday
      case Sunday => Saturday
        
    

