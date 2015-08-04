package org.virtuslab.beholder.filters.forms

import org.virtuslab.beholder.filters.{TabledFilterImplementation, FilterFactoryMethods}

class FormFilters[Entity] extends FilterFactoryMethods[Entity, FormFilterField, FormFormatter] {
  override def createFormatter(table: TabledFilterImplementation[_, _, _, FormFilterField[_, _], FormFormatter]): FormFormatter =
    new FormFormatter(table.filterFields, table.columnsNames)
}

