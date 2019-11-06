package crdt

import scala.annotation.tailrec


opaque type Ref = Long

object Ref
    def apply(raw: Long): Ref = raw
    inline val Init: Ref = 1 // A tombstone

given (ref: Ref)
    def raw: Long = ref


case class RGAElem[A](ref: Ref, value: A)


enum RGAOp[A]
    case Insert(elem: RGAElem[A], after: Ref)
    case Delete(ref: Ref)


object RefFactory
    inline val tombstoneBit = 1
    inline def limit(bits: Int): Long = Math.pow(2, bits.toDouble).toLong - 1
    inline def mask(value: Int, bits: Int): Long = value & limit(bits)
    inline def mask(value: Long, bits: Int): Long = value & limit(bits)


class RefFactory(nodeBits: Int)
    import RefFactory._

    val idBits = 64 - nodeBits - 1 // 2 = Init + tombstoneBit 

    val idLimit = limit(idBits)
    val nodeLimit = limit(nodeBits)

    def createRef(id: Long, node: Int, isTombstone: Boolean): Ref =
        val idPart = mask(id, idBits) << nodeBits << tombstoneBit
        val nodePart = mask(node, nodeBits) << tombstoneBit
        val tombstonePart = if isTombstone then 1 else 0

        Ref(idPart | nodePart | tombstonePart)

    def (ref: Ref) id: Long = ((ref.raw >> tombstoneBit >> nodeBits) & idLimit)
    def (ref: Ref) hasId(id: Long) = ref.id == id
    def (ref: Ref) node: Int = ((ref.raw >> tombstoneBit) & nodeLimit).toInt
    def (ref: Ref) hasNode(node: Int) = ref.node == node
    def (ref: Ref) isTombstone: Boolean = (ref.raw & 1) == 1
    def (ref: Ref) next: Ref = createRef(ref.id + 1, ref.node, ref.isTombstone)
    def (ref: Ref) equalsRef(that: Ref): Boolean =
        ref.id == that.id && ref.node == that.node
    def (ref: Ref) > (that: Ref): Boolean =
        ref.id > that.id || ref.node > that.node

    def refAsString(ref: Ref): String = 
        val id = ref.id
        val node = ref.node
        val isTombstone = if ref.isTombstone then '†' else '…'
        
        s"$id.$node.$isTombstone"

    def empty[A](node: Int): RGAList[A] = 
        RGAList(node, List(RGAElem(Ref.Init, null.asInstanceOf[A])))

    def fromList[A](node: Int, otherList: RGAList[A]): RGAList[A] = 
        RGAList(node, otherList.underlying)

    class RGAList[A](node: Int, val underlying: List[RGAElem[A]])
        def chain[B](f: RGAList[A] => (B => RGAOp[A]), value: B): RGAList[A] =
            val op = f(this)(value)
            merge(op)

        def append(value: A): RGAOp[A] =
            val elem = RGAElem(nextRef, value) 
            RGAOp.Insert(elem, underlying.last.ref)

        def prepend(value: A): RGAOp[A] =
            val elem = RGAElem(nextRef, value)
            RGAOp.Insert(elem, Ref.Init)

        def insertAfter(ref: Ref, value: A): RGAOp[A] =
            RGAOp.Insert(RGAElem(nextRef, value), ref)

        def delete(ref: Ref): RGAOp[A] = RGAOp.Delete(ref)

        private[this] def mergeInsert(elem: RGAElem[A], ref: Ref): List[RGAElem[A]] =
            if refs.contains(elem.ref) then
                underlying
            else
                ???

        def merge(rgaOp: RGAOp[A]): RGAList[A] =
            rgaOp match
                case RGAOp.Insert(elem, ref) =>
                    RGAList(node, mergeInsert(elem, ref))

                case RGAOp.Delete(ref) =>
                    val flipped = underlying.map { elem =>
                        if elem.ref == ref then
                            val tombstoneRef = createRef(
                                elem.ref.id,
                                elem.ref.node,
                                isTombstone = true
                            )

                            RGAElem(tombstoneRef, elem.value)
                        else
                            elem
                    }

                    RGAList(node, flipped)
        
        def refs: Iterator[Ref] = underlying.iterator.map(elem => elem.ref)
        def refAt(index: Int) = refs.drop(index).take(1).toList.head
        def nodeAt(index: Int) = iterator.drop(index).take(1).toList.head
        def values: Iterator[A] = underlying.iterator.map(elem => elem.value)
        def nextRef = 
            val maxRef = underlying.iterator.map(elem => elem.ref.id).max
            createRef(maxRef + 1, node, false)

        def iterator: Iterator[RGAElem[A]] = underlying.iterator

        def active = iterator.filterNot(elem => elem.ref.isTombstone)
        def deleted = iterator.filter(elem => elem.ref.isTombstone)

        override def toString: String =
            underlying.map{ elem =>
                val refString = refAsString(elem.ref)
                s"$refString=${elem.value}"
            }.mkString(s"RGAList<id=$node>(", ", ", ")")
        

@main def run() =
    val refFactory = RefFactory(8)

    val l1 = refFactory.empty[Char](0)
        .chain(_.append, 'a')
        .chain(_.append, 'b')
        .chain(_.append, 'c')

    println(l1)