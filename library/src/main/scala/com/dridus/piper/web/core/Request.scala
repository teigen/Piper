package com.dridus.piper.web.core

import java.io.{InputStream, Reader}
import com.dridus.piper.utils.hlist.{HList, HNil}

/** Enumeration of HTTP request methods */
object Method extends Enumeration {
    val GET     = Value("GET")
    val PUT     = Value("PUT")
    val DELETE  = Value("DELETE")
    val POST    = Value("POST")
    val HEAD    = Value("HEAD")
    val OPTIONS = Value("OPTIONS")
}

/** Basic HTTP request parameters */
final case class Request[+A](
    /** The HTTP request method used for this request */
    method: Method.Value,

    /** The "raw" string path that was requested */
    rawPath: String,

    /** The path being matched after being split by the usual path separator, / */
    fullPath: List[String],

    /** The path being matched after being split by the usual path separator, / */
    matchPath: List[String],

    /** The query string passed in the URL */
    queryString: String,

    /** The parameters passed in the URL or maybe in the body if some part of the pipeline has parsed the body as parameters */
    parameters: String => Option[List[String]],

    /** Whether the request was made securely (e.g. via HTTPS) */
    isSecure: Boolean,

    /** The client host address and port */
    client: () => (String, Int),

    /** The HTTP headers */
    headers: String => Option[List[String]],

    /** Additional attributes attached to the request */
    attributes: A
)

object Request {
    def splitPath(in: String): List[String] = in.split("/").toList.dropWhile(_ == "")
}

/** Attribute on a request that indciates that input can be obtained either as characters or as bytes */
final case class EitherRequestInput(
    toByteStream: () => ByteStreamInput,
    toCharStream: () => CharStreamInput
)

/** Abstract superclass of a request input type. Used as an attribute of Requests, after a particular input type has been chosen */
sealed abstract class RequestInput

object RequestInput {
    // /** Obtain the request input as a character stream */
    // FIXME need LensG or something to convert from input state vector A (... :*: EitherRequestInput :*: ...)
    // to output state vector B (... :*: CharStreamInput :*: ...)
    // def asCharacters[A <: HList](implicit lens: Lens[A: Request[
}

/** Request input in the form of a (possibly empty) input stream */
final case class ByteStreamInput(inputStream: InputStream) extends RequestInput

/** Request that has input in the form of a (possibly empty) reader */
final case class CharStreamInput(reader: Reader) extends RequestInput
