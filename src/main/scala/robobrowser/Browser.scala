package robobrowser

import java.net.URL

import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements
import requests.{BaseSession, Response, Session}
import robobrowser.forms.{Form, Submit}

import scala.collection.JavaConverters._

class State(val browser: Browser, val response: Response) {
  def url: String = response.url

  def parsed: Document = Jsoup.parse(response.text)
}

object Browser {
  protected def find(parsed: Document,
                     name: Option[String] = None,
                     attrs: Option[Map[String, String]],
                     recursive: Boolean = true,
                     text: Option[String] = None): Option[Element] = {

    findAll(parsed, name, attrs, recursive, text, Some(1)).headOption
  }

  protected def findAll(parsed: Document,
                        name: Option[String] = None,
                        attrs: Option[Map[String, String]],
                        recursive: Boolean = true, //TODO: don't know what to do with this
                        text: Option[String] = None,
                        limit: Option[Int] = None): List[Element] = {

    var result: List[Element] = null

    var selector: String = name getOrElse ""
    for ((name, value) <- attrs getOrElse Map[String, String]()) {
      selector += "[" + name + "=" + value + "]"
    }

    result = parsed.select(selector).asScala.toList

    if (text.nonEmpty) {
      result = result.filter(elem => elem.text().contains(text))
    }

    if (limit.nonEmpty) {
      result = result.take(limit.get)
    }

    result
  }
}

class Browser(_session: Option[Session] = None,
              userAgent: Option[String] = None,
              val history: Option[Int] = None,
              val timeout: Option[Int] = None,
              val allowRedirects: Boolean = true,
              cache: Boolean = false,
              cachePatterns: Option[List[String]] = None,
              maxAge: Option[Int] = None,
              tries: Option[Int] = None,
              multiplier: Option[Int] = None) {

  val headers: Map[String, String] = BaseSession.defaultHeaders map {
    case c@("User-Agent", _) => userAgent match {
      case Some(s) => ("User-Agent", s)
      case _ => c
    }
    case other => other
  }

  val session: Session = _session getOrElse Session(headers = this.headers)

  //TODO: add caching
  if (cache) {

  } else if (maxAge.nonEmpty) {
    throw new Exception("Parameter `maxAge` is provided, but caching is turned off")
  }

  private var states: List[State] = List[State]()
  private var cursor: Int = -1

  //TODO: set up retries


  override def toString: String = super.toString

  def state: State = cursor match {
    case -1 => throw new Exception("no state")
    case other => if (other < states.length) states(cursor) else throw new IndexOutOfBoundsException()
  }

  def response: Response = state.response

  def url: String = state.url

  def parsed: Document = state.parsed

  def select: String => Elements = parsed.select

  private def buildUrl(url: String): String = {
    val currentUrl = new URL(this.url)
    val newUrl = new URL(currentUrl, url)
    newUrl.toString
  }

  private def defaultSendArgs: Map[String, String] =
    Map("timeout" -> (timeout.getOrElse(10000) * 1000).toString, "allow_redirects" -> allowRedirects.toString)

  private def buildSendArgs(args: Map[String, String]): Map[String, String] = {
    defaultSendArgs ++ args
  }

  def open(url: String, method: String = "get", args: Option[Map[String, String]] = None): Unit = {
    val a = args getOrElse Map[String, String]()
    val response = method.toLowerCase match {
      case "get" => session.get(url, params = a)
      case "post" => session.post(url, data = a)
      case "put" => session.put(url, data = a)
      case "delete" => session.delete(url)
      case "head" => session.head(url)
      case "options" => session.options(url)
      case _ => throw new IllegalArgumentException("invalid method")
    }

    updateState(response)
  }

  private def updateState(response: Response): Unit = {
    states = states.dropRight(states.length - (cursor + 1))

    val newState = new State(this, response)
    states = states :+ newState
    cursor += 1

    if (history.nonEmpty) {
      val decrement = states.length - history.get
      if (decrement > 0) {
        states = states.drop(decrement)
        cursor -= decrement
      }
    }
  }

  private def traverse(n: Int = 1): Unit = {
    if (history.isEmpty) {
      throw new Exception("not tracking history")
    }

    val c = cursor + n
    if (c >= states.length || c < 0) {
      throw new Exception("trying to traverse too far")
    }

    cursor = c
  }

  def back(n: Int = 1): Unit = {
    traverse(-n)
  }

  def forward(n: Int = 1): Unit = {
    traverse(n)
  }

  def getLink(text: Option[String] = None, attrs: Option[Map[String, String]] = None): Option[Element] = {
    Browser.find(parsed, text = text, attrs = attrs, name = Some("a"))
  }

  def getLinks(text: Option[String] = None,
               attrs: Option[Map[String, String]] = None,
               limit: Option[Int] = None): List[Element] = {

    Browser.findAll(parsed, text = text, attrs = attrs, name = Some("a"), limit = limit)
  }

  def getForm(id: Option[String] = None, attrs: Option[Map[String, String]] = None): Option[Form] = {
    var attributes: Map[String, String] = attrs getOrElse Map[String, String]()
    if (id.nonEmpty) {
      attributes += ("id" -> id.get)
    }

    val formElem = Browser.find(parsed, attrs = Some(attributes), name = Some("form"))
    if (formElem.nonEmpty) {
      Some(new Form(formElem.get))
    } else {
      None
    }
  }

  def getForms(attrs: Option[Map[String, String]] = None): List[Form] = {
    val formElems: List[Element] = Browser.findAll(parsed, name = Some("form"), attrs = attrs)
    val forms: List[Form] = formElems.map(form => new Form(form))
    forms
  }

  def followLink(link: Element, args: Option[Map[String, String]] = None): Unit = {
    if (!link.hasAttr("href")) {
      throw new Exception("link must have `href` attribute")
    }

    val href = link.attr("href")
    open(buildUrl(href), args = args)
  }

  def submitForm(form: Form, submit: Option[Submit] = None, args: Option[Map[String, String]] = None): Unit = {
    val method = form.method
    var url = buildUrl(form.action)
    if (url.isEmpty) {
      url = this.url
    }
    val payload = form.serialize(submit).asMap
    val sendArgs = buildSendArgs(args getOrElse Map[String, String]()) ++ payload

    open(url, method, Some(sendArgs))
  }
}
