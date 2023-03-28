package u04lab.code
import Item.*
import List.*
import scala.Option.*

import scala.annotation.tailrec

object Lab4 extends App:
  object ListOfItems:
    private var itemsList: List[Item] = Nil()
    private def numberOfItems(t: Item*) =
      var itemsList: List[Item] = Nil()
      t.foreach(x => itemsList = append(itemsList, Cons(x, Nil())))
      itemsList
    private case class ListImpl(val itemList: List[Item])
    def apply(items: Item*): List[Item] = numberOfItems(items: _*)


  trait sameTag:
    def items: List[Item]

  object sameTag:
    @tailrec
    private def allTagsMatch(tags: List[String], t: String): Boolean = tags match
      case Cons(h, tail) if !h.matches(t) => allTagsMatch(tail, t)
      case Cons(h, tail) if h.matches(t) => true
      case Cons(_, Nil()) => false
      case Nil() => false

    def unapply(t: String): scala.Option[String] =
      val tags = flatMap(items)(_.tags)
      if (allTagsMatch(tags, t)) scala.Some(t)
      else scala.None

    def apply(i: List[Item]): sameTag = sameTagImpl(i)
    private case class sameTagImpl(override val items: List[Item]) extends sameTag


  val items = ListOfItems(Item(1, "item1", "tag1", "tag2"), Item(2, "item2", "tag1", "tag3"))
  val t = "tag2"

  t match
    case sameTag(tag) => println( s" $items have same tag $tag")
    case _ => println( s" $items have different tags ")


