package typelib

sealed trait HList {
  type Parent
  type This <: HListOf[Parent]

  val length: Int

  type Map[F[_ <: Parent] <: PP,PP] <: HListOf[PP]
  type FlatMap[F[_ <: Parent] <: Box[PP],PP] <: HListOf[PP]

  type ::[T <: Parent] = HNelOf[Parent,T,This]
  def foreach[U](f: Parent => U): Unit
}
sealed trait HListOf[P] extends HList with Traversable[P] {
  type Parent = P
  type This <: HListOf[P]
}
sealed trait HNel[P] extends HListOf[P] {
  type This = HNelOf[P,Top,Bottom]
  type Top <: P
  type Bottom <: HListOf[P]
  
  val top: Top
  val bottom: Bottom

  def ::[TT <: P](nextTop: TT): HNelOf[P,TT,This]
  def foreach[U](f: Parent => U) {
    f(top)
    bottom.foreach(f)
  }
}
case class HNelOf[P,T <: P, B <: HListOf[P]](top: T, bottom: B) extends HNel[P] {  
  type Top = T
  type Bottom = B

  type Map[F[_ <: P] <: PP,PP] = HNelOf[PP,F[Top],Bottom#Map[F,PP]]

  type FlatMap[F[_ <: P] <: Box[PP],PP] = ({
    type MapFunc[A <: PP] = HNelOf[PP,A,Bottom#FlatMap[F,PP]]
    type Res = F[Top]#Map[MapFunc,HListOf[PP]]#GetOrElse[Bottom#FlatMap[F,PP]]
  })#Res

  def ::[TT <: P](nextHead: TT) = HNelOf[P,TT,This](nextHead,this)

  final val length = bottom.length + 1
}
case class HNil[P]() extends HListOf[P] {
  type This = HNil[P]
  final val length = 0
  def ::[H <: P](top: H) = HNelOf[P,H,HNil[P]](top,this)

  type Map[F[_ <: P] <: PP,PP] = HNil[PP]
  type FlatMap[F[_ <: P] <: Box[PP],PP] = HNil[PP]

  def foreach[U](f: Parent => U) = Unit
}