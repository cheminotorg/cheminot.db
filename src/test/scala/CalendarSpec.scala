package org.cheminot.build.tests

import org.joda.time.DateTime
import org.cheminot.db._

class CalendarSpec extends CheminotSpec {

  behavior of "A calendar"

  it should "be equals to another calendar even if serviceId differs" in {
    val startDate = DateTime.now
    val endDate = startDate.plusMonths(1)
    val calendarA = Calendar(
      "serviceidA",
      monday = true,
      tuesday = true,
      wednesday = true,
      thursday = true,
      friday = true,
      saturday = false,
      sunday = false,
      startDate,
      endDate
    )
    assert(calendarA == calendarA.copy(serviceId = "serviceidB"))
  }
}
