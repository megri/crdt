package crdt

import scala.annotation.tailrec


opaque type Ref = Long

object Ref
    def apply(raw: Long): Ref = raw
    val init: Ref = RefFactory.tombstoneBit
    def end(given rgaList: RefFactory#RGAList[_]): Ref =
        rgaList.underlying.last.ref

given (ref: Ref)
    def raw: Long = ref
    def asTombstone: Ref = ref | RefFactory.tombstoneBit


case class RGAElem[A](ref: Ref, value: A)


enum RGAOp[A]
    case Insert(elem: RGAElem[A], after: Ref)
    case Delete(ref: Ref)


object RefFactory
    inline val tombstoneBit = 1
    inline def limit(bits: Int): Long = Math.pow(2, bits.toDouble).toLong - 1

class RefFactory(nodeBits: Int)
    import RefFactory._

    val idBits = 64 - nodeBits - tombstoneBit
    val idLimit = limit(idBits)
    val nodeLimit = limit(nodeBits)

    def createRef(id: Long, node: Int, isTombstone: Boolean): Ref =
        val idPart = (id & idLimit) << nodeBits << tombstoneBit
        val nodePart = (node & nodeLimit) << tombstoneBit
        val tombstonePart = if isTombstone then 1 else 0

        Ref(idPart | nodePart | tombstonePart)

    given refOrdering: Ordering[Ref] =
        Ordering.Long.on[Ref](_.id).orElseBy(_.node)

    def (ref: Ref) id: Long = ((ref.raw >> tombstoneBit >> nodeBits) & idLimit)
    def (ref: Ref) hasId(id: Long) = ref.id == id
    def (ref: Ref) node: Int = ((ref.raw >> tombstoneBit) & nodeLimit).toInt
    def (ref: Ref) hasNode(node: Int) = ref.node == node
    def (ref: Ref) isTombstone: Boolean = (ref.raw & 1) == 1

    def refAsString(ref: Ref): String = 
        val id = ref.id
        val node = ref.node
        val isTombstone = if ref.isTombstone then '†' else '…'
        
        s"$id.$node.$isTombstone"

    def empty[A](node: Int): RGAList[A] = 
        RGAList(node, List(RGAElem(Ref.init, null.asInstanceOf[A])))

    def fromList[A](node: Int, otherList: RGAList[A]): RGAList[A] = 
        RGAList(node, otherList.underlying)


    class RGAList[A](val node: Int, val underlying: List[RGAElem[A]])
        def insert(after: (given this.type) => Ref)(value: A): RGAOp[A] =
            RGAOp.Insert(RGAElem(maxRef.next, value), after(given this))

        def delete(ref: Ref): RGAOp[A] = RGAOp.Delete(ref)

        def chain(f: this.type => A => RGAOp[A], value: A): RGAList[A] =
            merge(f(this)(value))

        def merge(rgaOp: RGAOp[A]): RGAList[A] =
            rgaOp match
                case RGAOp.Insert(elem, ref) =>
                    RGAList(node, mergeInsert(elem, ref))

                case RGAOp.Delete(ref) =>
                    val updated = underlying.map { elem =>
                        if elem.ref == ref then
                            RGAElem(elem.ref.asTombstone, elem.value)
                        else
                            elem
                    }

                    RGAList(node, updated)
        
        def iterator: Iterator[RGAElem[A]] = underlying.iterator
        def refs: Iterator[Ref] = iterator.drop(1).map(elem => elem.ref)
        def values: Iterator[A] = iterator.drop(1).map(elem => elem.value)
        def maxRef: Ref = iterator.map(elem => elem.ref).max

        def active: Iterator[RGAElem[A]] =
            iterator.filterNot(elem => elem.ref.isTombstone)
        def deleted: Iterator[RGAElem[A]] =
            iterator.filter(elem => elem.ref.isTombstone)

        def (ref: Ref) next: Ref = createRef(ref.id + 1, node, false)

        override def toString: String =
            underlying.tail.map{ elem =>
                val refString = refAsString(elem.ref)
                s"$refString=${elem.value}"
            }.mkString(s"RGAList<id=$node>(", ", ", ")")

        private[this] def mergeInsert(elem: RGAElem[A], ref: Ref): List[RGAElem[A]] =
            if underlying contains elem then underlying
            else
                val after = underlying.dropWhile(x => !refOrdering.equiv(x.ref, ref))
                
                after match
                case refElem :: tail =>
                    val before = underlying.takeWhile(x => !refOrdering.equiv(x.ref, ref))
                    val (x, y) = tail.partition(x => refOrdering.gt(x.ref, elem.ref))
                    before ::: refElem :: x ::: elem :: y
            
                case Nil => underlying

@main def run() =
    val refFactory = RefFactory(8)
    val l1 = refFactory.empty[Char](0)
        .chain(_.insert(Ref.end), 'x')
        .chain(_.insert(Ref.end), 'y')
        .chain(_.insert(Ref.end), 'z')
        .chain(_.insert(Ref.init), 'a')

    println(l1)