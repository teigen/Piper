package com.dridus.piper.web

import scala.xml.NodeSeq
import scala.xml.Xhtml.toXhtml
import com.dridus.piper.utils.Error
import com.dridus.piper.web.core.{===>, CharStreamResponse, RedirectResponse, Request, Response}

/** Basic operations, like sending some fixed content */
package object basic {
    /** Send back some Response */
    def send[A](response: Response): A ===> Response = _ => Right(Some(response))

    /** Send some string back */
    def sendString(s: String, code: Int = 200, headers: Map[String, String] = Map.empty): Request[_] ===> Response =
        send(CharStreamResponse(headers + ("Content-Type" -> "text/plain"), code, _.write(s)))

    /** Send some XML back */
    def sendXML(ns: NodeSeq, code: Int = 200, headers: Map[String, String] = Map.empty): Request[_] ===> Response =
        send(CharStreamResponse(headers + ("Content-Type" -> "text/html"), code, _.write(toXhtml(ns))))

    /** Redirect somewhere */
    def redirect[A](to: String): A ===> Response = send(RedirectResponse(to))

    /** Handle missing resources */
    def notFound[A, B](handler: A ===> B): (A ===> B) => (A ===> B) =
        wrapping => in => wrapping(in).right.flatMap(_ match {
            case response@Some(_) => Right(response)
            case None => handler(in)
        })
}
