package kr.ac.kaist

/**
 * Created by ysko on 15. 7. 23..
 */
package object jsaf {
  implicit def any2waterfall[A](a: A): Object {def >>[B](f: (A) => B): B} = new AnyRef{
    def >>[B](f: A=>B) = f(a)
  }

}
