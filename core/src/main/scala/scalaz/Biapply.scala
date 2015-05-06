package scalaz

trait Biapply[F[_, _]] extends Bifunctor[F] { self =>
  def biap[A, B, C, D](fab: => F[A, B])(f: => F[A => C, B => D]): F[C, D]

  def compose[G[_, _]](implicit G0: Biapply[G]): Biapply[λ[(α, β) => F[G[α, β], G[α, β]]]] =
    new CompositionBiapply[F, G] {
      def F = self
      def G = G0
    }
  def product[G[_, _]](implicit G0: Biapply[G]): Biapply[λ[(α, β) => (F[α, β], G[α, β])]] =
    new ProductBiapply[F, G] {
      def F = self
      def G = G0
    }

  def biapply2[A, B, C, D, E, FF](fab: => F[A, B], fcd: => F[C, D])(f: (A, C) => E, g: (B, D) => FF): F[E, FF] =
    biap(fcd)(bimap(fab)(f.curried, g.curried))

  def bilift2[A, B, C, D, E, FF](f: (A, B) => C, g: (D, E) => FF): (F[A, D], F[B, E]) => F[C, FF] =
    biapply2(_, _)(f, g)
}

object Biapply {
  @inline def apply[F[_, _]](implicit F: Biapply[F]): Biapply[F] = F
}
