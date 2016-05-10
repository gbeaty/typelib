package typelib

trait Box[B] {
	type Map[F[_ <: B] <: BB,BB] <: Box[BB]
	type IsFull <: Bool
	type GetOrElse[E <: B] <: B
}
trait Full[A <: B,B] extends Box[B] {
	type Map[F[_ <: B] <: BB,BB] = Full[F[A],BB]
	type IsFull = True
	type GetOrElse[E <: B] = A
}
trait Empty[B] extends Box[B] {
	type Map[F[_ <: B] <: BB,BB] = Empty[BB]
	type IsFull = False
	type GetOrElse[E <: B] = E
}