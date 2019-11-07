import crdt._

val refFactory = RefFactory(8)

val l1 = refFactory.empty[Char](0)
val l2 = l1.chain(_.insert(Ref.end), 'x')
val l3 = l2.chain(_.insert(Ref.end), 'y')

