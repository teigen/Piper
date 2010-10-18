package com.dridus.piper.web.servlet

import javax.servlet
import javax.servlet.http
import scala.collection.JavaConversions.{asIterator, asMap}
import com.dridus.piper.utils.hlist.{:*:, HNil}
import com.dridus.piper.web.core.{
    ===>, ByteStreamInput, CharStreamInput, EitherRequestInput, Method, 
    Request, RedirectResponse, Response, StreamResponse
}

/**
 * Adapter trait that conforms a Pipeline to the Servlet model, and the starting point for most applications.
 */
trait Filter extends servlet.Filter {
    /** The pipeline to handle requests with */
    val handle: Request[EitherRequestInput :*: HNil] ===> Response

    def init(config: servlet.FilterConfig) = {}
    def destroy = {}
    def doFilter(servletRequest: servlet.ServletRequest, servletResponse: servlet.ServletResponse, chain: servlet.FilterChain): Unit =
        servletRequest match {
            case (httpServletRequest: http.HttpServletRequest) => {
                val httpServletResponse = servletResponse.asInstanceOf[http.HttpServletResponse]
                handle(makeRequest(httpServletRequest)) match {
                    case Left(exception) => throw exception
                    case Right(Some(response)) => updateResponse(httpServletResponse, response)
                    case Right(None) => chain.doFilter(servletRequest, servletResponse)
                }
            }

            case _ => chain.doFilter(servletRequest, servletResponse)
        }

    /** Create a Request from a HttpServletRequest */
    private def makeRequest(httpServletRequest: http.HttpServletRequest): Request[EitherRequestInput :*: HNil] = {
        lazy val parameters = Map(httpServletRequest.getParameterMap.toSeq.map {
            case (k: String, v: Array[String]) => (k, v.toList)
        }: _*)

        lazy val headers = Map(httpServletRequest.getHeaderNames.map { 
            case (n: String) => (n, httpServletRequest.getHeaders(n).asInstanceOf[java.util.Enumeration[String]].toList)
        }.toSeq: _*)

        val splitPath = Request.splitPath(httpServletRequest.getRequestURI)

        Request(
            method = Method.values.find(_.toString == httpServletRequest.getMethod).getOrElse(Method.GET),
            rawPath = httpServletRequest.getRequestURI,
            fullPath = splitPath,
            matchPath = splitPath,
            queryString = httpServletRequest.getQueryString,
            parameters = s => parameters.lift(s),
            isSecure = httpServletRequest.isSecure,
            client = () => (httpServletRequest.getRemoteAddr, httpServletRequest.getRemotePort),
            headers = s => headers.lift(s),
            attributes = EitherRequestInput(
                toByteStream = () => ByteStreamInput(httpServletRequest.getInputStream),
                toCharStream = () => CharStreamInput(httpServletRequest.getReader)
            ) :*: HNil
        )
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
