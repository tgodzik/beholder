package org.virtuslab.beholder.filters.dsl

import org.virtuslab.beholder.filters._
import org.virtuslab.beholder.filters.json.{JsonFilterField, JsonFormatter}
import org.virtuslab.unicorn.LongUnicornPlay.driver.simple._

import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}

class DSL[FT <: FilterField] {

  def crateFilter[T <: Product, E](
    query: Query[T, E, Seq],
    fields: Seq[FT],
    names: Seq[String]
  ) =
    FilterFactory.crate(query, fields, names)

  trait Mappable[S, E, NS]

  class EmptyName(name: String) extends Named[Unit](name)

  class Named[S](name: String) {
    def from[A, NS](c: Column[A])(implicit shaper: Mappable[S, A, NS]): NamedFieldColumn[NS, A] = ???
  }

  class CompleteRow[S] {
    def and(name: String): Named[S] = ???
  }

  class NamedFieldColumn[S, A] {
    def as[B](field: MappedFilterField[A, B] with FT): CompleteRow[S] = ???
  }

  implicit def string2EmptyName(name: String): EmptyName = ???

  def create[T, L, E, TableE](query: Query[T, L, Seq])(creation: T => CompleteRow[E]): MappableFilterAPI[E, Unit, FT, TableE] = macro DSLImpl.create_imp

  //generated mappable implicits

  implicit def unitShape[A]: Mappable[Unit, A, Tuple1[A]] = ???

  implicit def tuple1Shape[A, B]: Mappable[Tuple1[A], B, (A, B)] = ???

  implicit def tuple2Shape[A1, A2, A3]: Mappable[(A1, A2), A3, (A1, A2, A3)] = ???

  implicit def tuple3Shape[A1, A2, A3, A4]: Mappable[(A1, A2, A3), A4, (A1, A2, A3, A4)] = ???

  implicit def tuple4Shape[A1, A2, A3, A4, A5]: Mappable[(A1, A2, A3, A4), A5, (A1, A2, A3, A4, A5)] = ???

  implicit def tuple5Shape[A1, A2, A3, A4, A5, A6]: Mappable[(A1, A2, A3, A4, A5), A6, (A1, A2, A3, A4, A5, A6)] = ???

  implicit def tuple6Shape[A1, A2, A3, A4, A5, A6, A7]: Mappable[(A1, A2, A3, A4, A5, A6), A7, (A1, A2, A3, A4, A5, A6, A7)] = ???

}

object JsonDSL extends DSL[JsonFilterField] {

  implicit class JsonFieldedFilter[E <: Product, Formatter, FT <: JsonFilterField, T](filter: MappableFilterAPI[E, Formatter, FT, T]) {
    def jsonFormatted[NE <: Product](mapping: E => NE, names: String => String): FilterAPI[NE, JsonFormatter[NE]] =
      filter.mapped(mapping).withFormat(filter => new JsonFormatter[NE](filter.filterFields, filter.columnsNames, names))

    def asJson[NE <: Product](applyFun: E => NE) = jsonFormatted(applyFun, identity)

    def asJson = jsonFormatted(identity, identity)
  }

}