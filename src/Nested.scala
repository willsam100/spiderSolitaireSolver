/**
 * Created by willsam100 on 13/02/14.
 */
trait Nested[A] {
  def length(a: A): Int
  def clone(a: A): A
}

object Nested {
  implicit def nested[A] = new Nested[A] {
    def length(a: A): Int = 1
    def clone(a: A): A = a
  }

  implicit def nestedArray[A](implicit ev: Nested[A], mf: Manifest[A]) = new Nested[Array[A]] {
    def length(as: Array[A]): Int = as.map { a => ev.length(a) }.sum
    def clone(as: Array[A]): Array[A] = as.map { a => ev.clone(a) }
  }
}

