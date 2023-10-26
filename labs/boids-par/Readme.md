# Boids, in parallel

In this week's callback, you will look back at the Boids lab, where you did
everything without polymorphism and Scala collections, and change the
computation to use Scala collections, and identify opportunities to parallelize
the code.

## Obtaining the lab files

Run the following commands to obtain a copy of the lab:

```console
$ git clone https://gitlab.epfl.ch/lamp/cs-214/boids-par.git
$ cd boids-par
```

The provided code contains a possible solution to the previous lab. You will
need to modify the `BoidLogic.scala` file to complete this lab.

## Step 1: Moving to a new collection

Since our homemade `BoidSequence` doesn't provide a parallel interface yet,
we're going to have to switch to a different collection for now.

Start by replacing instances of `BoidSequence` with `Vector[Boid]`. What other
changes do you have to make to the previous solution now that you have access to
a polymorphic collection?

## Step 2: Boids flying out in parallel

With the modified implementation, can you find opportunities for
parallelization? With some careful analysis, you can gain a lot of performance
by adding a `.par` and `.seq` before and after your operations.

<details class = "note check">

Why do you need both the `.par` and the `.seq`?

<summary>Solution</summary>

`(v: Vector[Boid]).par` produces a `ParVector[Boid]`. Performing `map`, etc. on
it will return a parallel vector back where possible. However, since the
external interface expects to see a `Vector[Boid]`, and not a `ParVector[Boid]`,
you will have to convert the collection back to a `Vector[Boid]`. This can be
done with a `.seq` or a `.toVector`.

```scala
  val isVector1: Vector[Int] = Vector(1, 2, 3).par            // ❌
  // Found:    scala.collection.parallel.immutable.ParVector[Int]
  // Required: Vector[Int]

  val isVector2: Vector[Int] = Vector(1, 2, 3).par.seq        // ✔️
  val isVector3: Vector[Int] = Vector(1, 2, 3).par.toVector   // ✔️
```

</details>

Before jumping into the modifications, spend some time analyzing the sequential
code. Identify all the places where independent calculations are being made one
after the other.

It is most efficient to start from the "top" of your computation. The larger the
computation you can split up, the better! This is akin to dividing a big project
into its major components first, rather than starting with the tiny details. By
splitting the largest independent chunks of the computation, we can maximize the
amount of work done in parallel with the least number of threads created. 

<details class = "hint">

<summary>Hint</summary>

You can parallelize `tickWorld` as the force on one boid is independent of the
force on another boid in a given step.

</details>

Remember, not all parts of the code might be suitable for parallelization.
Sometimes, the overhead of parallel execution might outweigh its benefits. Test,
test, and test your code again!


## Testing

As changing the representation of the data should not affect the behavior of the
program, all the previous test suites should continue to pass. 

You can run all suites as usual with:

```console
$ sbt test
```

To test the performance of your code as you play with it, start by commenting the relevant line in `src/main/scala/boids/Benchmarks.scala`:

```scala
class Benchmarks:
  @Benchmark
  def tickWorldComplete1K =
    val allBoids = generateRandomBoids(1000)
    // UNCOMMENT THIS WHEN YOU HAVE CONVERTED EVERYTHING TO VECTORS
    // tickWorld(allBoids, defaultPhysics)
```

The benchmark is written with `Vector[Boid]`, so it would not have compiled before your change.

You can run the benchmark by typing `Jmh/run -t max -f 1` in `sbt`. `-t max`
asks to make the maximum number of threads available, and `-f 1` asks to run
only one fork, i.e., run the whole benchmark suite for only one iteration, just
to make it faster.

You can write your own functions inside this and tag them `@Benchmark` to test
them as well.

You can run specific benchmarks with their name as `Jmh/run -t max -f 1
Benchmarks.tickWorldComplete1K`. Note that adding any non-benchmark members to
the `Benchmarks` class will probably result in errors. Use the `Benchmarks`
*object* for this.

## Submit your patch!

Once you are done, you should:

- Commit your work to your local repository,
- Submit a patch named `lab.patch` to Moodle with your changes set.

