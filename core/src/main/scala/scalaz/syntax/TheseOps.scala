package scalaz
package syntax

final class TheseOps[A](self: A) {
  final def wrapThis[B]: A \&/ B =
    \&/.This(self)

  final def `this`[B]: A \&/ B =
    \&/.This(self)

  final def wrapThat[B]: B \&/ A =
    \&/.That(self)

  final def that[B]: B \&/ A =
    \&/.That(self)
}

final class ThesePairOps[A, B](self: (A, B)) {
  final def both: A \&/ B =
    \&/.Both(self._1, self._2)
}

trait ToTheseOps {
  implicit def ToTheseOps[A](a: A): TheseOps[A] = new TheseOps(a)
  implicit def ToThesePairOps[A, B](a: (A, B)): ThesePairOps[A, B] = new ThesePairOps(a)
}
