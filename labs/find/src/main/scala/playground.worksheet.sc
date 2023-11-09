import find.findByNameAndPrint
import find.findAllAndPrint
import find.cs214.open

val food = open("example-dir/food")

food.path()

food.hasChildren()

val fruits = food.firstChild()

fruits.path()

fruits.hasNextSibling()

fruits.nextSibling().path()

findByNameAndPrint(food, "project")

val l = List(Option(1), Option(2), Option(3), Option(None), Option(5))

l.flatMap(x => x)