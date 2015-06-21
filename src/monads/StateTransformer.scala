/**
 * The State Transformer Monad in Scala.
 *
 * Created by ppgllrd on 21/06/15.
 */


package monads

class ST[S,A](val fst : S => (S,A)) {
  
  def map[B](f : A => B) : ST[S,B] =
    new ST((st : S) => {
      val (s1,x) = fst(st)
      (s1, f(x))
    })

  def flatMap[B](f : A => ST[S,B]) : ST[S,B] =
    new ST((s : S) => {
      val (s1,x) = fst(s)
      val st = f(x)
      st.fst(s1)
    })

  def run(st : S) : A = this.fst(st)._2

  def >>=[B](f : A => ST[S,B]) : ST[S,B] = this.flatMap(f)

  def >>[B](st : ST[S,B]) : ST[S,B] = this.flatMap(_ => st)
}

object ST {
  def pure[S,A](x : A) : ST[S,A] = new ST(s => (s,x))

  def read[S] : ST[S,S] = new ST(s => (s,s))

  def write[S](s : S) : ST[S,Unit] = new ST(_ => (s,Unit))
}


object Demo extends App {
  import ST._

  val ej =
    write(10) >>
    read >>= { (y:Int) =>
    pure(y)
    }


  val v = ej.run(0)

  println(v)


  val ej1 =
    for(_ <- write(10);
        _ <- write(20);
        x <- pure(5);
        z <- read) yield (x,z)


  val v1 = ej1.run(0)

  println(v1)

}







































