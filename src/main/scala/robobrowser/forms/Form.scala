package robobrowser.forms

import com.sun.javaws.exceptions.InvalidArgumentException
import org.jsoup.nodes.Element
import org.jsoup.select.Elements
import robobrowser.collection.SortedMultiMap

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object Form {
  private def groupFlatTag(elem: Element, elems: Elements): List[Element] = {
    val name = elem.attr("name").toLowerCase
    val filtered: mutable.Buffer[Element] = for (e: Element <- elems.asScala if name == e.attr("name").toLowerCase) yield e

    List[Element](elem) ::: filtered.toList
  }

  private def parseField(elem: Element, elems: Elements): BaseField[_] = elem.tagName().toLowerCase match {
    case "input" => elem.attr("type") match {
      case "submit" => Submit(List[Element](elem))
      case "file" => FileInput(List[Element](elem))
      case "radio" => val radios = groupFlatTag(elem, elems); Radio(radios)
      case "checkbox" => val checkboxes = groupFlatTag(elem, elems); Checkbox(checkboxes)
      case _ => Input(List[Element](elem))
    }
    case "textarea" => TextArea(List[Element](elem))
    case "select" => if (elem.hasAttr("multiple")) MultiSelect(List[Element](elem)) else Select(List[Element](elem))
    case _ => throw new Exception("don't care about other types.")
  }

  protected def parseFields(elem: Element): List[BaseField[_]] = {
    val tags_selector = "input, textarea, select"
    val tags = elem.select(tags_selector)
    for (elem: Element <- tags.asScala) {
      elem.tagName(elem.tagName().toLowerCase)
    }

    val out: mutable.ListBuffer[BaseField[_]] = ListBuffer[BaseField[_]]()
    while (!tags.isEmpty) {
      val elem: Element = tags.remove(0)
      try {
        out.append(parseField(elem, tags))
      } catch {
        case _: Exception => {
          /*nothing*/
        }
      }
    }

    out.toList
  }

  private def filterFields(fieldsMap: SortedMultiMap[String, BaseField[_]],
                           x: BaseField[_] => Boolean): SortedMultiMap[String, BaseField[_]] = {

    val map = new SortedMultiMap[String, BaseField[_]]
    for ((name: String, field: BaseField[_]) <- fieldsMap.items if x(field)) map.addBinding(name, field)

    map
  }

  protected def prepareFields(allFields: SortedMultiMap[String, BaseField[_]],
                              submitFields: SortedMultiMap[String, BaseField[_]],
                              submit: Option[Submit] = None): SortedMultiMap[String, BaseField[_]] = {

    var result: SortedMultiMap[String, BaseField[_]] = allFields

    if (submitFields.items.size > 1) {

      if (submit.isEmpty) {
        throw new Exception("invalid submit")
      }

      val sfs: mutable.Set[BaseField[_]] = submitFields.getOrElse(submit.get.name, mutable.Set[BaseField[_]]())
      val names: mutable.Set[String] = sfs.map(field => field.name)

      if (!names.contains(submit.get.name)) {
        throw new Exception("invalid submit")
      }

      result = filterFields(allFields, field => !field.isInstanceOf[Submit] || field == submit)
    }

    result
  }
}

class Form(val parsed: Element) {
  if (parsed.tagName().toLowerCase != "form") {
    throw new InvalidArgumentException(Array[String]("parsed"))
  }

  val action: String = parsed.attr("action")
  val method: String = if (parsed.attr("method") == "") "get" else parsed.attr("method")
  val fields = new SortedMultiMap[String, BaseField[_]]
  for (field <- Form.parseFields(this.parsed)) {
    this.addField(field)
  }

  def addField(field: BaseField[_]): Unit = {
    fields.addBinding(field.name, field)
  }

  def submitFields: SortedMultiMap[String, BaseField[_]] =
    Form.filterFields(this.fields, field => field.isInstanceOf[Submit])

  def keys: List[String] = fields.keys.toList

  def get(item: String): Option[mutable.Set[BaseField[_]]] = fields.get(item)

  def set(key: String, value: String): Unit = {
    val fieldOption = fields.get(key)
    if (fieldOption.isEmpty) {
      throw new Exception("field does not exist")
    }

    val field = fieldOption.get
    val first: BaseField[String] = field.head.asInstanceOf[BaseField[String]]
    first.value = value
  }

  def serialize(submit: Option[Submit] = None): Payload = {
    val prepared = Form.prepareFields(fields, submitFields, submit)
    Payload.fromFields(prepared)
  }

  override def toString: String = {
    val fieldStr = fields.items.map { case (name, field) => name + "=" + field.value }.mkString(", ")
    "<RoboForm %s>".format(fieldStr)
  }
}

object Payload {
  def fromFields(fieldsMap: SortedMultiMap[String, BaseField[_]]): Payload = {
    val payload = new Payload
    for {
      (_, field) <- fieldsMap.items
      if !field.isDisabled
    } payload.add(field.serialize, field.payloadKey)

    payload
  }
}

class Payload {
  val data = new SortedMultiMap[String, String]
  val options = new SortedMultiMap[String, SortedMultiMap[String, String]]

  def add(data: List[(String, String)], payloadKey: Option[String] = None): Unit = {
    if (payloadKey.isEmpty) {
      for ((name: String, value: String) <- data) {
        this.data.addBinding(name, value)
      }
    } else {
      //TODO
    }
  }

  def asMap: Map[String, String] = data.items.toMap
}
