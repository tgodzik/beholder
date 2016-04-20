package org.virtuslab.beholder.filters.json

import org.virtuslab.beholder.filters.{ FilterAPI, FilterDefinition, FilterResult }
import org.virtuslab.unicorn.LongUnicornPlay.driver.api._
import play.api.libs.json._
import play.api.mvc._
import slick.dbio.DBIO

import scala.concurrent.{ Future, ExecutionContext }

trait FilterControllerBase[Entity <: Product] extends Controller {
  private[beholder] def formatter: JsonFormatter[Entity]

  private[beholder] def callFilter(filterDefinition: FilterDefinition): DBIO[FilterResult[Entity]]

  implicit def executionContext: ExecutionContext

  protected def inFilterContext(body: Request[AnyContent] => DBIO[JsResult[JsValue]]): EssentialAction

  final def filterDefinition = inFilterContext { request => DBIO.successful(JsSuccess(formatter.jsonDefinition)) }

  final def doFilter: EssentialAction =
    inFilterContext {
      request =>
        request.body.asJson.map(formatter.filterDefinition).map {
          case JsSuccess(filterDefinition, path) =>
            val filterResultAction = callFilter(mapFilterData(filterDefinition))
            filterResultAction.map {
              filterResult =>
                val formatResults = formatter.results(filterDefinition, modifyFilterResults(filterResult, filterDefinition))
                JsSuccess(formatResults, path)
            }

          case other =>
            DBIO.successful(JsError("json expected"))
        }.getOrElse(DBIO.successful(JsError("json expected")))
    }

  //for filter modification such us setting default parameters etc.
  protected def mapFilterData(data: FilterDefinition) = data

  //for result modification such as sorting or fetching additional data
  protected def modifyFilterResults(results: FilterResult[Entity], filterDefinition: FilterDefinition) = results
}

abstract class FilterController[Entity <: Product](filter: FilterAPI[Entity, JsonFormatter[Entity]])
    extends FilterControllerBase[Entity] {

  override protected def inFilterContext(body: (Request[AnyContent]) => DBIO[JsResult[JsValue]]): EssentialAction

  override final private[beholder] def callFilter(filterDefinition: FilterDefinition): DBIO[FilterResult[Entity]] =
    filter.filterWithTotalEntitiesNumber(filterDefinition)

  override final private[beholder] def formatter: JsonFormatter[Entity] = filter.formatter
}