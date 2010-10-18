package com.dridus.piper.utils

/**
 * Hetereogeneous lists, encoded as cons cells + nil terminator in the type system.
 * Similar in some ways to a n-ary tuple.
 *
 * Adapted from code by Paytronix
 * Adapted from code by Jesper Nordenberg
 */
object hlist {
    /** The supertype of HLists */
    sealed trait HList {
        /** Convert an HList to an erased list */
        def toList: List[Any]
    }

    /** The "Nil" for HLists */
    final class HNil private[hlist] () extends HList {
        /** Produce a new NTuple with the given element as the sole one */
        def :*: [A] (lhs: A): A :*: HNil = HCons(lhs, this)

        def toList = Nil

        override def toString = "HNil"
    }

    /** Nil for HLists. Done this way rather than using objects because of Scala's refusal to infer singleton types */
    val HNil = new HNil

    /** Cons cell of HLists, with one element anqd the rest of the HList */
    final case class HCons[B, C <: HList](head: B, tail: C) extends HList {
        /** Cons another thing on to the HList */
        def :*: [A] (lhs: A): A :*: B :*: C = HCons(lhs, this)

        def toList = head :: tail.toList

        override def toString = head.toString + " :*: " + tail.toString
    }

    /** Type alias for HCons which makes typing naming and construction more succint */
    type :*: [A, B <: HList] = HCons[A, B]

    /** Extractor object so that HList can be deconstructed in match statements */
    object :*: {
        def unapply[A, B <: HList](in: A :*: B): Option[(A, B)] = Some(in.head, in.tail)
    }

    /** Making HLists act as type-indexed products. Essentially a type-system reified Map[Type => Value] */
    object tip {
        /** Implicitly provide lenses on type indexed products, where the target type is the head */
        implicit def lens[A, B <: HList]: Lens[A :*: B, A] = Lens(
            project = r => r.head,
            inject = (r, v) =>  HCons(v, r.tail)
        )

        /** Implicitly provide lenses on type indexed products, where the target type is somewhere in the tail */
        implicit def derivedLens[A, B <: HList, C](implicit next: Lens[B, C]): Lens[A :*: B, C] = Lens(
            project = r => next.project(r.tail),
            inject = (r, v) => HCons(r.head, next.inject(r.tail, v))
        )
    }
}
