package boids
import cs214.{Vector2, BoidSequence, BoidCons, BoidNil}

def boidsWithinRadius(thisBoid: Boid, boids: BoidSequence, radius: Float): BoidSequence =
  boids.filter(b => (b != thisBoid) && (b.position.distanceTo(thisBoid.position) < radius)) // Keeps only boids within radius

def avoidanceForce(thisBoid: Boid, boidsWithinAvoidanceRadius: BoidSequence): cs214.Vector2 =
  boidsWithinAvoidanceRadius.foldLeftVector2(Vector2.Zero)((acc, other) => { // Sum of all forces
    val dist = thisBoid.position.distanceTo(other.position)
    val direction = (thisBoid.position - other.position).normalized
    val force = if dist == 0 then Vector2.Zero else direction / dist

    if dist == 0 then acc else acc + force
  })

def cohesionForce(thisBoid: Boid, boidsWithinPerceptionRadius: BoidSequence): cs214.Vector2 =
  if boidsWithinPerceptionRadius.length > 0 then
    val posSum = boidsWithinPerceptionRadius.foldLeftVector2(Vector2.Zero)((acc, other) => {
    acc + other.position
  }) // Recursively sum all positions
    val meanPosition = posSum / boidsWithinPerceptionRadius.length.toFloat

    meanPosition - thisBoid.position
  else
    cs214.Vector2.Zero

def alignmentForce(thisBoid: Boid, boidsWithinPerceptionRadius: BoidSequence): cs214.Vector2 =
  if boidsWithinPerceptionRadius.length > 0 then
    val velSum = boidsWithinPerceptionRadius.foldLeftVector2(Vector2.Zero)((acc, other) => {
    acc + other.velocity
  }) // Recursively sum all velocities
    val meanPosition = velSum / boidsWithinPerceptionRadius.length.toFloat

    meanPosition - thisBoid.velocity
  else
    cs214.Vector2.Zero

def containmentForce(thisBoid: Boid, allBoids: BoidSequence, width: Int, height: Int): Vector2 = {
  val x = if thisBoid.position.x < 0 then 1 else if thisBoid.position.x > width then -1 else 0
  val y = if thisBoid.position.y < 0 then 1 else if thisBoid.position.y > height then -1 else 0
  Vector2(x.toFloat, y.toFloat)
  }

def totalForce(thisBoid: Boid, allBoids: BoidSequence, physics: Physics): Vector2 =
  val withinPerceptionRadius = boidsWithinRadius(thisBoid, allBoids, physics.perceptionRadius)
  val cohere = cohesionForce(thisBoid, withinPerceptionRadius)
  val align = alignmentForce(thisBoid, withinPerceptionRadius)
  val withinAvoidanceRadius = boidsWithinRadius(thisBoid, withinPerceptionRadius, physics.avoidanceRadius)
  val avoid = avoidanceForce(thisBoid, withinAvoidanceRadius)
  val contain = containmentForce(thisBoid, allBoids, physics.WIDTH, physics.HEIGHT)
  val total =
    avoid * physics.avoidanceWeight +
      cohere * physics.cohesionWeight +
      align * physics.alignmentWeight +
      contain * physics.containmentWeight
  total


def tickBoid(thisBoid: Boid, allBoids: BoidSequence, physics: Physics): Boid =
  val acceleration = totalForce(thisBoid, allBoids, physics)    
  val velocity = clampVelocity(thisBoid.velocity + acceleration, physics)               // v+1 = v + a
  val position = thisBoid.position + thisBoid.velocity          // x+1 = x + v
  Boid(position, velocity)

def tickWorld(allBoids: BoidSequence, physics: Physics): BoidSequence =
  allBoids.mapBoid(boid => tickBoid(boid, allBoids, physics))  // Apply tickBoid to all boids

def clampVelocity(speed: Vector2, physics: Physics): Vector2 = 
  val maxSpeed = physics.maximumSpeed
  val minSpeed = physics.minimumSpeed

  if speed.norm > maxSpeed then
    speed.normalized * maxSpeed
  else if speed.norm < minSpeed then
    speed.normalized * minSpeed
  else
    speed
  