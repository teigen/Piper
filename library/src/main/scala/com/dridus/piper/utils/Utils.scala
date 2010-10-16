package com.dridus.piper

package object utils {
    /** Either a Throwable indicating failure, or a value */
    type Error[A] = Either[Throwable, A]
}
