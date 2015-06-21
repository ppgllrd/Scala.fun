/**
 * The Maybe Monad in Scala.
 *
 * Created by ppgllrd on 21/06/15.
 */

package monads


sealed trait Maybe[+A] {
  def map[B](f : A => B) : Maybe[B]
  def flatMap[B](fm : A => Maybe[B]) : Maybe[B]
  def run : A
  def _catch[C>:A](fm : String => Maybe[C]) : Maybe[C]
}

case class Fail(msg : String) extends Maybe[Nothing] {
  def map[B](f : Nothing => B) = Fail(msg)

  def flatMap[B](fm : Nothing => Maybe[B]) : Maybe[B] = Fail(msg)

  def run : Nothing = sys.error(msg)

  def _catch[C](fm : String => Maybe[C]) : Maybe[C] = fm(msg)
}

case class Success[A](x : A) extends Maybe[A] {
  def map[B](f: A => B): Maybe[B] = Success(f(x))

  def flatMap[B](fm: A => Maybe[B]): Maybe[B] = fm(x)

  def run : A = x

  def _catch[C>:A](fm : String => Maybe[C]) : Maybe[C] = this
}

object Maybe {
  def pure[A](x : A) : Maybe[A] = Success(x)

  def fail[A](msg : String) : Maybe[A] = Fail(msg)
}

object DemoExc extends App {

  import Maybe._

  def div(x : Int, y : Int) : Maybe[Int] =
    if(y==0) fail("Division by zero") else pure(x/y)

  val ex1 =
    for(x <- pure(10);
        y <- pure(0);
        z <- div(x,y)
    ) yield z

  val ex2 = ex1 _catch {msg => { println(msg); pure(0) }}

  println(ex2.run)
}