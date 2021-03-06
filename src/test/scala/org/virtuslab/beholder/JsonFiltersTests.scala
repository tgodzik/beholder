package org.virtuslab.beholder

import java.sql.Date

import org.virtuslab.beholder.filters.json.JsonFilterFields.inField
import org.virtuslab.beholder.filters.json.JsonFilterFields.inIntFieldSeq
import org.virtuslab.beholder.filters.json.JsonFilterFields.inText
import org.virtuslab.beholder.filters.json.JsonFilterFields.{ inIntField, inOptionRange, inRange, inEnum, inEnumSeq }
import org.virtuslab.beholder.filters.json.{ JsonFilterFields, JsonFilters, JsonFormatter }
import org.virtuslab.beholder.filters.{ FilterAPI, FilterDefinition }
import org.virtuslab.beholder.model.MachineStatus
import org.virtuslab.beholder.suites._
import org.virtuslab.unicorn.LongUnicornPlay.driver.simple._
import play.api.libs.json.{ JsSuccess, JsObject }

trait JsonFiltersTestsBase {
  self: AppTest with BaseSuite[JsonFormatter[UserMachineViewRow]] =>

  override def doFilters(data: BaseFilterData, currentFilter: FilterDefinition): Seq[UserMachineViewRow] = {
    import data._
    val result = filter.filterWithTotalEntitiesNumber(currentFilter)

    filter.formatter.results(currentFilter, result) match {
      case JsObject(Seq(("filter", jsonFilter), _)) =>
        filter.formatter.filterDefinition(jsonFilter) should equal(JsSuccess(currentFilter))
    }

    result.content
  }
}

class JsonFiltersTests extends AppTest with FiltersTestSuite[JsonFormatter[UserMachineViewRow]] with JsonFiltersTestsBase {
  def createFilter(data: BaseFilterData): FilterAPI[UserMachineViewRow, JsonFormatter[UserMachineViewRow]] =
    new JsonFilters[UserMachineViewRow](identity).create(
      data.view,
      inText,
      inText,
      inIntField,
      inRange(inField[Date]("date")),
      JsonFilterFields.ignore[Option[BigDecimal]],
      inEnum(MachineStatus)
    )
}

class InitialJsonFiltersTests extends JsonFiltersTests with InitialQueryTestSuite[JsonFormatter[UserMachineViewRow]]

class JsonFiltersRangeTests extends AppTest with RangeFiltersSuite[JsonFormatter[UserMachineViewRow]] with JsonFiltersTestsBase {
  def createFilter(data: BaseFilterData): FilterAPI[UserMachineViewRow, JsonFormatter[UserMachineViewRow]] =
    new JsonFilters[UserMachineViewRow](identity).create(
      data.view,
      inText,
      inText,
      inRange(inIntField),
      inRange(inField[Date]("date")),
      inOptionRange(inField[BigDecimal]("number")),
      inEnum(MachineStatus)
    )
}

class JsonFiltersEnumTests extends AppTest with EnumFilterTestSuite[JsonFormatter[UserMachineViewRow]] with JsonFiltersTestsBase {
  def createFilter(data: BaseFilterData): FilterAPI[UserMachineViewRow, JsonFormatter[UserMachineViewRow]] =
    new JsonFilters[UserMachineViewRow](identity).create(
      data.view,
      inText,
      inText,
      inIntFieldSeq,
      inRange(inField[Date]("date")),
      inOptionRange(inField[BigDecimal]("number")),
      inEnum(MachineStatus)
    )
}

class JsonFiltersSeqTests extends AppTest with SeqFilterTestSuite[JsonFormatter[UserMachineViewRow]] with JsonFiltersTestsBase {
  def createFilter(data: BaseFilterData): FilterAPI[UserMachineViewRow, JsonFormatter[UserMachineViewRow]] =
    new JsonFilters[UserMachineViewRow](identity).create(
      data.view,
      inText,
      inText,
      inIntFieldSeq,
      inRange(inField[Date]("date")),
      inOptionRange(inField[BigDecimal]("number")),
      inEnumSeq(MachineStatus)
    )
}