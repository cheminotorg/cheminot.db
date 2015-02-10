package m.cheminot.misc

import scalaj.http.{ Http, Token }
import java.net.URLEncoder

case class TwitterOAuth(consumerKey: String, consumerSecret: String, accessTokenKey: String, accessTokenSecret: String)

object Twitter {

  def updateStatus(oauth: TwitterOAuth, status: String) = {
    val consumerToken = Token(oauth.consumerKey, oauth.consumerSecret)
    val accessToken = Token(oauth.accessTokenKey, oauth.accessTokenSecret)
    val url = "https://api.twitter.com/1.1/statuses/update.json"
    Http(url).postForm(Seq("status" -> status)).oauth(consumerToken, accessToken).asString
  }
}
