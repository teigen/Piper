package com.dridus.piper.web.core

import java.io.{OutputStream, OutputStreamWriter, Writer}

/** Abstract supertype of a basic web response which serves as the final output of the web pipeline */
sealed trait Response

/** Class of responses which cause a redirect */
case class RedirectResponse(to: String) extends Response

/** Trait of responses which emit some data to the client via an OutputStream */
trait StreamResponse extends Response {
    /** HTTP headers to send to the client */
    val headers: Map[String, String]

    /** HTTP status code to send to the client */
    val code: Int

    /** Stream the output to the client */
    def writeTo(outputStream: OutputStream): Unit
}

/** Class of responses which emit some data to the client in the form of a stream of bytes */
case class ByteStreamResponse(headers: Map[String, String], code: Int, streamer: OutputStream => Unit) extends StreamResponse {
    def writeTo(outputStream: OutputStream) = {
        streamer(outputStream)
        outputStream.flush
    }
}

/** Class of responses which emit some data to the client in the form of a stream of characters */
case class CharStreamResponse(val headers: Map[String, String], val code: Int, val streamer: Writer => Unit) extends StreamResponse {
    def writeTo(outputStream: OutputStream) = {
        val writer = new OutputStreamWriter(outputStream)
        streamer(writer)
        writer.flush
    }
}
