package org.cheminot.db

import rapture.http._
import rapture.net._

package object http {

  def BadRequest[A](body: String) =
    ErrorResponse(400, Nil, "BadRequest", body)

  def isPostReq(request: HttpRequest): Boolean =
    request.requestMethod match {
      case HttpMethods.Post => true
      case _ => false
    }
}
