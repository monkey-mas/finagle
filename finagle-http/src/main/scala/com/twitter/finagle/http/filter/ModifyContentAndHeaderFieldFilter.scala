package com.twitter.finagle.http.filter

import com.twitter.finagle.http.{Fields, Request, Response, Status}
import com.twitter.finagle._
import com.twitter.util.Future

/**
 * Modify Content and header field filter. Remove message-body and modify a Content-Length header field if necessary.
 *
 * @see [[https://tools.ietf.org/html/rfc2616 "Hypertext Transfer Protocol -- HTTP/1.1"]]
 * @see [[https://tools.ietf.org/html/rfc7230 "Hypertext Transfer Protocol (HTTP/1.1): Message Syntax and Routing"]]
 * section-3.3.2 on `Content-Length` for more information.
 */
private[finagle] class ModifyContentAndHeaderFieldFilter[Req <: Request] extends SimpleFilter[Req, Response] {
  import ModifyContentAndHeaderFieldFilter._

  def apply(request: Req, service: Service[Req, Response]): Future[Response] = {
    service(request) map { rep =>
      if (mustNotIncludeContent(rep.status)) {
        rep.clearContent()

        if (rep.status == Status.NoContent)
          rep.headerMap.remove(Fields.ContentLength)

        //Make sure a Content-Length header field is set with a value of 0 if it is explicitly sent for a 304 response
        if (rep.contentLength.isDefined)
          rep.headerMap.set(Fields.ContentLength, "0")
      }

      rep
    }
  }
}

private[finagle] object ModifyContentAndHeaderFieldFilter extends HeadFilter[Request] {

  val Role = Stack.Role("ModifyContentAndHeaderField")
  val Description = "Remove message-body and modify a Content-Length header field if necessary"

  private def mustNotIncludeContent(status: Status): Boolean = status match {
    case Status.NoContent | Status.NotModified => true
    case _ => false
  }

  def module: Stackable[ServiceFactory[Request, Response]] =
    new Stack.Module0[ServiceFactory[Request, Response]] {
      val role = Role
      val description = Description

      def make(next: ServiceFactory[Request, Response]): ServiceFactory[Request, Response] = {
        new ModifyContentAndHeaderFieldFilter().andThen(next)
      }
    }
}
