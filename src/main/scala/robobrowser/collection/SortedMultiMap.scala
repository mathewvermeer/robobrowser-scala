package robobrowser.collection

import scala.collection.mutable


class SortedMultiMap[A, B] extends mutable.LinkedHashMap[A, mutable.Set[B]]() with mutable.MultiMap[A, B] {
  def items: List[(A, B)] = keys.flatMap(key => findEntry(key).value.map(value => (key, value))).toList
}