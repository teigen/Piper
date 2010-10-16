package com.dridus.piper.web

import com.dridus.piper.utils.Error
import com.dridus.piper.web.core.{===>, Request, Response}

/**
 * Paths provides a path matching DSL.
 *
 * import com.dridus.piper.web.Paths
 *
 * Paths (
 *   "foo" / "bar" / {
 *     case "baz" => handle foo/bar/baz/...
 *     case "zip" => handle foo/bar/zip/...
 *   },
 *   "qux" / (handle qux/...),
 *   "bub" {
 *     case "one" => handle bub/one (but not bub/one/foo)
 *     case "two" => handle bub/two (but not bub/two/foo)
 *   }
 * )
 */
object Paths {
    /** Combine all the given pipelines alternatives */
    def apply[A](alternatives: Request ===> A*): Request ===> A =
        in => alternatives.foldLeft[Error[Option[A]]](Right(None))((prev, f) => prev match {
            case Right(None) => f(in)
            case other => other
        })

    /** Object that can be used to apply partial functions to the root, or dispatch for no path segments */
    object root {
        /** Dispatch to the given pipeline if the path to match is empty */
        def apply[A](rhs: Request ===> A): Request ===> A =
            in => if (in.path.isEmpty) rhs(in)
                  else Right(None)

        /** Dispatch to the given pipeline if the path to match is empty */
        def apply[A](rhs: PartialFunction[String, Request ===> A]): Request ===> A = dispatchPF(rhs)

        /**
         * Delegate to one of the given pipelines to match the head path segment
         * 
         * E.g.
         * root / {
         *   case "baz" => handle foo/bar/baz
         *   case "zippy" => handle foo/bar/zippy
         * }
         */
        def / [A](rhs: PartialFunction[String, Request ===> A]): Request ===> A = delegatePF(rhs)
    }

    /** Add operators applicable to a path segment */
    implicit def pathSegmentOps(in: String): PathSegmentOps = new PathSegmentOps(in)

    /**
     * Operators on strings that match the string as a path segment:
     *    "path" / ...  delegates (/ as in subdirectory)
     *    "path" f      dispatches
     */
    final class PathSegmentOps(lhs: String) {
        /** Delegate to the given pipeline if the current path segment is equal to the string */
        def / [A](rhs: Request ===> A): Request ===> A = 
            in => in.path match {
                case head :: rest if head == lhs => rhs(in.copy(path=rest))
                case _ => Right(None)
            }

        /** Assemble a sequence of path segments */
        def / (rhs: String): PathPrefixOps = new PathPrefixOps(lhs :: rhs :: Nil)

        /**
         * Delegate to one of the given pipelines to match the path segment after the matching head segment.
         * 
         * E.g.
         * "foo" / {
         *   case "baz" => handle foo/bar/baz
         *   case "zippy" => handle foo/bar/zippy
         * }
         */
        def / [A](rhs: PartialFunction[String, Request ===> A]): Request ===> A = this / delegatePF(rhs)

        /** Dispatch to the given pipeline if the current path segment is equal to the string */
        def -> [A](rhs: Request ===> A): Request ===> A =
            in => in.path match {
                case head :: rest if head == lhs => rhs(in)
                case _ => Right(None)
            }

        /**
         * Dispatch to one of the given pipelines to match the path segment after the matching head segment.
         * 
         * E.g.
         * "foo" -> {
         *   case "baz" => handle foo/bar/baz
         *   case "zippy" => handle foo/bar/zippy
         * }
         */
        def -> [A](rhs: PartialFunction[String, Request ===> A]): Request ===> A = this / dispatchPF(rhs)
    }

    /** Add operators applicable to a path prefix */
    implicit def pathPrefixOps(in: List[String]): PathPrefixOps = new PathPrefixOps(in)

    /**
     * Operators of lists of strings that match that prefix off the path:
     *    "foo" / "bar" / "baz" / pipeline    delegates
     *    "foo" / "bar" / "baz" -> pipeline   dispatches
     */
    final class PathPrefixOps(lhs: List[String]) {
        /** Delegate to the given pipeline if the current path has the prefix */
        def / [A](rhs: Request ===> A): Request ===> A =
            in => if (in.path.startsWith(lhs)) rhs(in.copy(path=in.path.drop(lhs.length)))
                  else Right(None)

        /**
         * Delegate to one of the given pipelines to match the path segment after the prefix.
         * 
         * E.g.
         * "foo" / "bar" / {
         *   case "baz" => handle foo/bar/baz
         *   case "zippy" => handle foo/bar/zippy
         * }
         */
        def / [A](rhs: PartialFunction[String, Request ===> A]): Request ===> A = this / delegatePF(rhs)

        /** Dispatch to the given pipeline if the current path has the prefix */
        def -> [A](rhs: Request ===> A): Request ===> A =
            in => if (in.path.startsWith(lhs)) rhs(in.copy(path=in.path.drop(lhs.length)))
                  else Right(None)

        /**
         * Dispatch to one of the given pipelines to match the path segment after the prefix.
         * 
         * E.g.
         * "foo" / "bar" -> {
         *   case "baz" => handle foo/bar/baz
         *   case "zippy" => handle foo/bar/zippy
         * }
         */
        def -> [A](rhs: PartialFunction[String, Request ===> A]): Request ===> A = this / dispatchPF(rhs)
    }

    /**
     * Delegate by (head) path segment using a partial function -- differs from dispatching in that the matching path
     * segment is removed when applying to the matching rule.
     *
     * E.g.
     * delegate {
     *   case "foo" => pipeline
     *   case "bar" => pipeline
     *   case "baz" => pipeline
     */
    def delegatePF[A](pf: PartialFunction[String, Request ===> A]): Request ===> A =
        in => in.path match {
            case head :: rest => pf.lift(head).map(_(in.copy(path=rest))) getOrElse Right(None)
            case Nil          => Right(None)
        }

    /** Dispatch by path using a partial function. Only matches if there is only one path segment remaining. */
    def dispatchPF[A](pf: PartialFunction[String, Request ===> A]): Request ===> A =
        in => in.path match {
            case head :: _ => pf.lift(head).map(_(in)) getOrElse Right(None)
            case Nil       => Right(None)
        }
}
