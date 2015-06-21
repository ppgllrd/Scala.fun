/**
 * The State Transformer Monad in Scala.
 *
 * Created by ppgllrd on 21/06/15.
 */


package monads

case class ST[S,+A](fst : S => (S,A)) {
  
  def map[B](f : A => B) : ST[S,B] =
    ST((st : S) => {
      val (s1,x) = this.fst(st)
      (s1, f(x))
    })

  def flatMap[B](f : A => ST[S,B]) : ST[S,B] =
    ST((s : S) => {
      val (s1,x) = this.fst(s)
      val st = f(x)
      st.fst(s1)
    })

  def run(st : S) : A = this.fst(st)._2

  def >>=[B](f : A => ST[S,B]) : ST[S,B] = this.flatMap(f)

  def >>[B](st : ST[S,B]) : ST[S,B] = this.flatMap(_ => st)
}

object ST {
  def pure[S,A](x : A) : ST[S,A] = ST(s => (s,x))

  def read[S] : ST[S,S] = ST(s => (s,s))

  def write[S](s : S) : ST[S,Unit] = ST(_ => (s,Unit))
}


object DemoST extends App {
  import ST._

  val ex =
    write(10) >>
    read >>= { (y:Int) =>
    pure(y)
    }


  val v = ex.run(0)

  println(v)


  val ex1 =
    for(_ <- write(10);
        _ <- write(20);
        x <- pure(5);
        z <- read) yield (x,z)


  val v1 = ex1.run(0)

  println(v1)
}
