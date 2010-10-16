package com.dridus.piper.web

import com.dridus.piper.utils.Error

package object core {
    /**
     * Type of pipeline segments which consume some input and can in reaction fail (Left(throwable)), succeed with some result (Right(Some(x)))
     * or opt not to handle the input (Right(None))
     */
    type ===>[A, B] = A => Error[Option[B]]
}

package core {
    object Pipeline {
        /** Implicitly extend pipeline functions with composition operations */
        implicit def pipelineOps[A, B, C, D](in: (B ===> C) => (A ===> D)): PipelineOps[A, B, C, D] =
            new PipelineOps(in)
    }

    /** Extended operations on pipeline functions which allow for composition */
    final class PipelineOps[A, B, C, D](lhs: (B ===> C) => (A ===> D)) {
        /** Application as an operator, to make pipeline expressions not be a huge nested morass of parentheses */
        def ===> (rhs: B ===> C): A ===> D = lhs(rhs)
    }
}
