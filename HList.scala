package typelib

trait HList {
  type Base
  type This <: HListOf[Base]

  val length: Int

  type Map[F[_ <: Base] <: BB,BB] <: HListOf[BB]
  type FlatMap[F[_ <: Base] <: Box[BB],BB] <: HListOf[BB]

  type ::[H <: Base] = HNelOf[Base,H,This]
}
trait HListOf[B] extends HList {
  type Base = B
  type This <: HListOf[B]

  // type ::[H <: Base] = HNelOf[Base,H,this.type]
}
trait HNel[B] extends HListOf[B] {
  type This = HNelOf[B,Head,Tail]
  type Head <: B
  type Tail <: HListOf[B]
  
  val head: Head
  val tail: Tail

  final val length = tail.length + 1

  def ::[HH <: B](nextHead: HH): HNelOf[B,HH,This]
}
case class HNelOf[B,H <: B, T <: HListOf[B]](head: H, tail: T) extends HNel[B] {  
  type Head = H
  type Tail = T

  type Map[F[_ <: B] <: BB,BB] = HNelOf[BB,F[Head],Tail#Map[F,BB]]

  type FlatMap[F[_ <: B] <: Box[BB],BB] = ({
    type MapFunc[A <: BB] = HNelOf[BB,A,Tail#FlatMap[F,BB]]
    type Res = F[Head]#Map[MapFunc,HListOf[BB]]#GetOrElse[Tail#FlatMap[F,BB]]
  })#Res

  def ::[HH <: B](nextHead: HH) = HNelOf[B,HH,This](nextHead,this)
}
case class HNil[B]() extends HListOf[B] {
  type This = HNil[B]
  final val length = 0
  def ::[H <: B](head: H) = HNelOf[B,H,HNil[B]](head,this)

  type Map[F[_ <: B] <: BB,BB] = HNil[BB]
  type FlatMap[F[_ <: B] <: Box[BB],BB] = HNil[BB]
}