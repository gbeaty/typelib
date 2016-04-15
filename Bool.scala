package typelib

sealed trait Bool {
	type If[T,E]
	type And[B <: Bool] <: Bool
	type Or[B <: Bool] <: Bool
}
trait False extends Bool {
	type If[T,E] = E
	type And[B <: Bool] = False
	type Or[B <: Bool] = B
}
trait True extends Bool {
	type If[T,E] = T
	type And[B <: Bool] = B
	type Or[B <: Bool] = True
}