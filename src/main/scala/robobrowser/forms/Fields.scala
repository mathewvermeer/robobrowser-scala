package robobrowser.forms

import java.io.File

import org.jsoup.nodes.Element

import scala.collection.JavaConverters._

trait BaseField[A] {
  val parsed: List[Element]
  protected var _value: Option[A] = None
  val name: String = getName(parsed)
  val payloadKey: Option[String] = None

  def value: A

  def value_=(value: A): Unit = _value = Option(value)

  protected def getName(p: List[Element]): String = {
    val elem = p.head
    if (elem.attr("name") == "")
      throw new Exception("Invalid Name Exception")

    elem.attr("name")
  }

  def isDisabled: Boolean = parsed.head.attributes.hasKey("disabled")

  def serialize: List[(String, String)]
}

case class Input(parsed: List[Element]) extends BaseField[String] {
  value = parsed.head.`val`()

  override def value: String = _value getOrElse ""

  override def serialize: List[(String, String)] = List((name, value))
}

case class Submit(parsed: List[Element]) extends BaseField[String] {
  value = parsed.head.`val`()

  override def value: String = _value getOrElse ""

  override def serialize: List[(String, String)] = List((name, value))
}

case class FileInput(parsed: List[Element]) extends BaseField[File] {
  override val payloadKey: Option[String] = Option("files")

  override def value: File = _value get

  override def serialize: List[(String, String)] = List((name, value.toString))
}

trait MultiOptionField[A] extends BaseField[A] {
  var options: List[String] = List[String]()
  var labels: List[String] = List[String]()
  var initial: List[String] = List[String]()
  loadOptions(parsed)
  setInitial(initial)

  protected def loadOptions(parsed: List[Element])

  protected def setInitial(initial: List[String])

  protected def valueToIndex(value: String): Int = {
    if (options.contains(value))
      return options.indexOf(value)
    else if (labels.contains(value))
      return labels.indexOf(value)

    throw new Exception("Option not found in field")
  }
}

trait FlatOptionField[A] extends MultiOptionField[A] {
  override def isDisabled: Boolean = parsed.forall(_.hasAttr("disabled"))

  override protected def loadOptions(parsed: List[Element]): Unit = {
    val options: List[String] = for (option: Element <- parsed) yield {
      if (option.`val`() != "") option.`val`() else "on"
    }

    val labels: List[String] = for (option: Element <- parsed) yield {
      if (!option.nextSibling().toString.isEmpty)
        option.nextSibling().toString
      else if (!option.previousSibling().toString.isEmpty)
        option.previousSibling().toString
      else ""
    }

    val initial: List[String] = for (option: Element <- parsed if option.hasAttr("checked")) yield {
      option.`val`()
    }

    this.options = options
    this.labels = labels
    this.initial = initial
  }
}

trait NestedOptionField[A] extends MultiOptionField[A] {
  override def isDisabled: Boolean =
    parsed.forall(_.hasAttr("disabled")) || parsed.head.select("option").asScala.forall(_.hasAttr("disabled"))

  override protected def loadOptions(parsed: List[Element]): Unit = {
    val optionsToScalaList =  parsed.head.select("option").asScala.toList

    val options: List[String] = for (option: Element <- optionsToScalaList) yield {
      if (option.`val`() != "") option.`val`() else option.text()
    }

    val labels: List[String] = for (option: Element <- optionsToScalaList) yield option.text()

    val initial: List[String] =
      for (option: Element <- optionsToScalaList if option.hasAttr("selected")) yield {
        if (option.`val`() != "") option.`val`() else option.text()
      }

    this.options = options
    this.labels = labels
    this.initial = initial
  }
}

case class TextArea(parsed: List[Element]) extends BaseField[String] {
  this.value = parsed.head.text().stripLineEnd

  override def value: String = _value getOrElse ""

  override def serialize: List[(String, String)] = List((name, value))
}

case class Checkbox(parsed: List[Element]) extends FlatOptionField[List[String]] {
  override protected def setInitial(initial: List[String]): Unit = this.value = initial

  override def value: List[String] = _value getOrElse List[String]()

  override def serialize: List[(String, String)] = for (v <- value) yield (name, v)
}

case class Radio(parsed: List[Element]) extends FlatOptionField[String] {
  override protected def setInitial(initial: List[String]): Unit = {
    _value = if (initial.nonEmpty) Option(initial.head) else None
  }

  override def value: String = _value getOrElse ""

  override def serialize: List[(String, String)] = List((name, value))
}

case class Select(parsed: List[Element]) extends NestedOptionField[String] {
  override protected def setInitial(initial: List[String]): Unit = {
    _value = if (initial.nonEmpty) Option(initial.head) else None

    if (_value.isEmpty && options.nonEmpty) {
      this.value = options.head
    }
  }

  override def value: String = _value getOrElse ""

  override def serialize: List[(String, String)] = List((name, value))
}

case class MultiSelect(parsed: List[Element]) extends NestedOptionField[List[String]] {
  override protected def setInitial(initial: List[String]): Unit = this.value = initial

  override def value: List[String] = _value getOrElse List[String]()

  override def serialize: List[(String, String)] = for (v <- value) yield (name, v)
}
