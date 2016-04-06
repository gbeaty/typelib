package typelib

trait HList[B] {
  type Self <: HList[B]
  val length: Int    

  type ::[H <: B] = HNelOf[B,H,this.type]

  type Map[F[_ <: B] <: BB,BB] <: HList[BB]
}
trait HNel[B] extends HList[B] {
  type Self = HNelOf[B,Head,Tail]
  type Head <: B
  type Tail <: HList[B]
  
  val head: Head
  val tail: Tail

  final val length = tail.length + 1  

  def ::[HH <: B](nextHead: HH): HNelOf[B,HH,Self]
}
case class HNelOf[B,H <: B, T <: HList[B]](head: H, tail: T) extends HNel[B] {  
  type Head = H
  type Tail = T

  type Map[F[_ <: B] <: BB,BB] = HNelOf[BB,F[Head],Tail#Map[F,BB]]  

  def ::[HH <: B](nextHead: HH) = HNelOf[B,HH,Self](nextHead,this)
}
case class HNil[B]() extends HList[B] {
  type Self = HNil[B]
  final val length = 0
  def ::[H <: B](head: H) = HNelOf[B,H,HNil[B]](head,this)

  type Map[F[_ <: B] <: BB,BB] = HNil[BB]
}