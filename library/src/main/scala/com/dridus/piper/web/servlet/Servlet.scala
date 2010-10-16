package com.dridus.piper.web.servlet

import javax.servlet
import javax.servlet.http
import scala.collection.JavaConversions.{asIterator, asMap}
import com.dridus.piper.web.core.{===>, ByteStreamInput, Method, Request, RedirectResponse, Response, StreamResponse}

/**
 * Adapter trait that conforms a Pipeline to the Servlet model, and the starting point for most applications.
 */
trait Filter extends servlet.Filter {
    /** The pipeline to handle requests with */
    val handle: Request ===> Response

    def init(config: servlet.FilterConfig) = {}
    def destroy = {}
    def doFilter(servletRequest: servlet.ServletRequest, servletResponse: servlet.ServletResponse, chain: servlet.FilterChain): Unit =
        servletRequest match {
            case (httpServletRequest: http.HttpServletRequest) => {
                val httpServletResponse = servletResponse.asInstanceOf[http.HttpServletResponse]
                handle(new ServletRequest(httpServletRequest)) match {
                    case Left(exception) => throw exception
                    case Right(Some(response)) => updateResponse(httpServletResponse, response)
                    case Right(None) => chain.doFilter(servletRequest, servletResponse)
                }
            }

            case _ => chain.doFilter(servletRequest, servletResponse)
        }

    /** Update the HttpServletResponse using the result of processing the pipeline */
    private def updateResponse(httpServletResponse: http.HttpServletResponse, response: Response): Unit =
        response match {
            case RedirectResponse(to) => httpServletResponse.sendRedirect(to)
            case (sr: StreamResponse) => {
                httpServletResponse.setStatus(sr.code)
                sr.headers.foreach {
                    case (name, value) => httpServletResponse.addHeader(name, value)
                }
                sr.writeTo(httpServletResponse.getOutputStream)
            }
        }
}


/** Concrete implementation of a Piper Request that adapts a HttpServletRequest */
final class ServletRequest(val httpServletRequest: http.HttpServletRequest) extends Request {
    val method = Method.values.find(_.toString == httpServletRequest.getMethod).getOrElse(Method.GET)
    val rawPath = httpServletRequest.getRequestURI
    val queryString = httpServletRequest.getQueryString
    val isSecure = httpServletRequest.isSecure
    lazy val client = (httpServletRequest.getRemoteAddr, httpServletRequest.getRemotePort)
    val input = ByteStreamInput(httpServletRequest.getInputStream)

    lazy val queryParameters = Map(httpServletRequest.getParameterMap.toSeq.map {
        case (k: String, v: Array[String]) => (k, v.toList)
    }: _*)

    lazy val headers = Map(httpServletRequest.getHeaderNames.map { 
        case (n: String) => (n, httpServletRequest.getHeaders(n).asInstanceOf[java.util.Enumeration[String]].toList)
    }.toSeq: _*)
}
