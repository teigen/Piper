package com.dridus.piper.web.core

import java.io.{InputStream, Reader}

/** Enumeration of HTTP request methods */
object Method extends Enumeration {
    val GET     = Value("GET")
    val PUT     = Value("PUT")
    val DELETE  = Value("DELETE")
    val POST    = Value("POST")
    val HEAD    = Value("HEAD")
    val OPTIONS = Value("OPTIONS")
}

/** Trait with the most basic of HTTP request parameters */
trait Request {
    /** The HTTP request method used for this request */
    val method: Method.Value

    /** The "raw" string path that was requested */
    val rawPath: String

    /** The path being matched after being split by the usual path separator, / */
    lazy val path: List[String] = rawPath.split("/").toList.dropWhile(_ == "")

    /** The query string passed in the URL */
    val queryString: String

    /** The query parameters passed in the URL or maybe in the body if some part of the pipeline has parsed the body as parameters */
    val queryParameters: Map[String, List[String]]

    /** Whether the request was made securely (e.g. via HTTPS) */
    val isSecure: Boolean

    /** The client host address and port */
    val client: (String, Int)

    /** The HTTP headers */
    val headers: Map[String, List[String]]

    /** The input */
    val input: RequestInput

    /** Copy this request with potentially some things changed */
    def copy(
        method:          Method.Value                 = this.method,
        rawPath:         String                       = this.rawPath,
        path:            => List[String]              = this.path,
        queryString:     String                       = this.queryString,
        queryParameters: => Map[String, List[String]] = this.queryParameters,
        isSecure:        Boolean                      = this.isSecure,
        client:          => (String, Int)             = this.client,
        headers:         => Map[String, List[String]] = this.headers,
        input:           RequestInput                 = this.input
    ): Request = {
        // ye gods, aliasing. can someone please prove me retarded and clean this up somehow?
        val _method = method
        val _rawPath = rawPath
        lazy val _path = path
        val _queryString = queryString
        lazy val _queryParameters = queryParameters
        val _isSecure = isSecure
        lazy val _client = client
        lazy val _headers = headers
        val _input = input

        new Request {
            val method = _method
            val rawPath = _rawPath
            override lazy val path = _path
            val queryString = _queryString
            lazy val queryParameters = _queryParameters
            val isSecure = _isSecure
            lazy val client = _client
            lazy val headers = _headers
            val input = _input
        }
    }
}

/** Abstract superclass of request input types */
sealed abstract class RequestInput

/** Request input in the form of a (possibly empty) input stream */
final case class ByteStreamInput(inputStream: InputStream) extends RequestInput

/** Request that has input in the form of a (possibly empty) reader */
final case class CharStreamInput(reader: Reader) extends RequestInput
