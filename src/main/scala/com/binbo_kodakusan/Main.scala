// https://wandbox.org/permlink/jqNHjKRonyRxYeQk
// https://wandbox.org/permlink/UKaaA4A9GS8aaJM8
// https://wandbox.org/permlink/rs4yrRiDnXyshbxm

sealed trait Nat {
  def fold[T](f: T => T)(a: T): T
  def toInt: Int

  def plus(rhs: Nat): Nat =
    this match {
      case Succ(n) => Succ(n.plus(rhs))
      case Zero() =>
        rhs match {
          case Succ(n) => rhs.plus(Zero())
          case Zero() => rhs
        }
    }
  
  def minus(rhs: Nat): Nat =
    (this, rhs) match {
       case (Succ(m), Succ(n)) => m.minus(n)
       case _ => this
    }
}

object Nat {
  def fromInt(n: Int): Nat = {
    if (n <= 0)
      Zero()
    else
      Succ(fromInt(n - 1))
  }
}

case class Succ(pre: Nat) extends Nat {
  def fold[T](f: T => T)(a: T): T = f(pre.fold(f)(a))
  def toInt: Int = fold[Int](_ + 1)(0)
}

case class Zero() extends Nat {
  def fold[T](f: T => T)(a: T): T = a
  def toInt: Int = 0
}

object Main extends App {
  val two = Nat.fromInt(2)
  val three = Nat.fromInt(3)
  println(s"${two.toInt} = $two, ${three.toInt} = $three")
  println(s"${two.toInt} + ${three.toInt} = ${two.plus(three).toInt}")
  println(s"${three.toInt} + ${two.toInt} = ${three.minus(two).toInt}")
}