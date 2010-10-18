package com.dridus.piper.utils

/** A lens, with a projection function and an injection/update function */
final case class Lens[Record, Value](project: Record => Value, inject: (Record, Value) => Record) {
    /** Project the field from the record */
    def apply(in: Record): Value = project(in)

    /** Inject a new value into the record */
    def update(record: Record, value: Value): Record = inject(record, value)
}

object Lens {
    /** Identity lens, where Value =:= Record */
    implicit def identityLens[A]: Lens[A, A] = Lens(r => r, (r, v) => v)
}
