package robobrowser.forms

import org.jsoup.nodes.Element

import scala.collection.JavaConverters._

trait BaseField {
  val parsed: List[Element]
  val name: String = getName(parsed)
  val payloadKey: Option[String] = None

  protected def getName(p: List[Element]): String = {
    val elem = p.head
    if (elem.attr("name") == "")
      throw new Exception("Invalid Name Exception")

    elem.attr("name")
  }

  def isDisabled: Boolean = parsed.head.attributes.hasKey("disabled")

  def serialize: List[(String, String)]
}

trait SingleValueField extends BaseField {
  protected var _value: Option[String] = None

  def value: String = _value getOrElse ""

  def value_=(value: String): Unit = this._value = Some(value)
}

trait MultiValueField extends BaseField {
  protected var _value: Option[List[String]] = None

  def value: List[String] = _value getOrElse List[String]()

  def value_=(value: List[String]): Unit = this._value = Some(value)
}

trait MultiOptionField extends BaseField {
  var options: List[String] = Nil
  var labels: List[String] = Nil
  var initial: List[String] = Nil
  loadOptions(parsed)
  setInitial(initial)

  protected def loadOptions(parsed: List[Element])

  protected def setInitial(initial: List[String])

  protected def getValueFromString(value: String): String = {
    var index = -1
    if (options.contains(value))
      index = options.indexOf(value)
    else if (labels.contains(value))
      index = labels.indexOf(value)

    if (index == -1)
      throw new Exception("Option not found in field")

    options(index)
  }
}

trait FlatOptionField extends MultiOptionField {
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

trait NestedOptionField extends MultiOptionField {
  override def isDisabled: Boolean =
    parsed.forall(_.hasAttr("disabled")) || parsed.head.select("option").asScala.forall(_.hasAttr("disabled"))

  override protected def loadOptions(parsed: List[Element]): Unit = {
    val optionsToScalaList = parsed.head.select("option").asScala.toList

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

case class Input(parsed: List[Element]) extends SingleValueField {
  value = parsed.head.`val`()

  override def serialize: List[(String, String)] = List((name, value))
}

case class Submit(parsed: List[Element]) extends SingleValueField {
  value = parsed.head.`val`()

  override def serialize: List[(String, String)] = List((name, value))
}

case class TextArea(parsed: List[Element]) extends SingleValueField {
  value = parsed.head.`val`()

  override def serialize: List[(String, String)] = List((name, value))
}

case class Checkbox(parsed: List[Element]) extends FlatOptionField with MultiValueField {
  override def value_=(value: List[String]): Unit = _value = Some(value.map(v => getValueFromString(v)))

  override protected def setInitial(initial: List[String]): Unit = this.value = initial

  override def serialize: List[(String, String)] = value.map(v => (name, v))
}

case class Radio(parsed: List[Element]) extends FlatOptionField with SingleValueField {
  override def value_=(value: String): Unit = _value = Some(getValueFromString(value))

  override protected def setInitial(initial: List[String]): Unit = {
    _value = if (initial.nonEmpty) Option(initial.head) else None
  }

  override def serialize: List[(String, String)] = List((name, value))
}

case class Select(parsed: List[Element]) extends NestedOptionField with SingleValueField {
  override def value_=(value: String): Unit = _value = Some(getValueFromString(value))

  override protected def setInitial(initial: List[String]): Unit = {
    _value = if (initial.nonEmpty) Option(initial.head) else None

    if (_value.isEmpty && options.nonEmpty) {
      this.value = options.head
    }
  }

  override def serialize: List[(String, String)] = List((name, value))
}

case class MultiSelect(parsed: List[Element]) extends NestedOptionField with MultiValueField {
  override def value_=(value: List[String]): Unit = _value = Some(value.map(v => getValueFromString(v)))

  override protected def setInitial(initial: List[String]): Unit = this.value = initial

  override def serialize: List[(String, String)] = value.map(v => (name, v))
}

case class FileInput(parsed: List[Element]) extends SingleValueField {
  override val payloadKey: Option[String] = Some("files")

  override def serialize: List[(String, String)] = List((name, value.toString))
}
