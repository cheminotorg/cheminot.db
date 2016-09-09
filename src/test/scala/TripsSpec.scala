package org.cheminot.build.tests

import org.scalatest._
import org.cheminot.misc
import org.cheminot.db._

abstract class CheminotSpec extends FlatSpec

class TripsSpec extends CheminotSpec {

  behavior of "A list of trips"

  it should "merge two trips that have the same stoptimes" in {
    val distinctAB = Builder.distinctTrips(List(Mock.tripA, Mock.tripB)).map(_.id)
    assert(distinctAB == List(Mock.tripA.id))
  }

  it should "merge two trips if the one them contains the other one" in {
    val distinctABC = Builder.distinctTrips(List(Mock.tripA, Mock.tripB, Mock.tripC)).map(_.id)
    assert(distinctABC == List(Mock.tripA).map(_.id))

    val distinctACB = Builder.distinctTrips(List(Mock.tripA, Mock.tripC, Mock.tripB)).map(_.id)
    assert(distinctACB == List(Mock.tripA).map(_.id))
  }
}

object Mock {

  val tripA = Trip(
    "tripA",
    "xxx",
    Calendar.off,
    None,
    List(
      StopTime(
        "OCESN862418F0100545013",
        misc.DateTime.parseOrFail("2016-02-05T17:02:06.232+01:00"),
        misc.DateTime.parseOrFail("2016-02-05T17:02:06.232+01:00"),
        "8739400",

        0
      ),
      StopTime(
        "OCESN862418F0100545013",
        misc.DateTime.parseOrFail("2016-02-05T17:42:06.232+01:00"),
        misc.DateTime.parseOrFail("2016-02-05T17:53:06.232+01:00"),
        "8739099",
        1
      ),
      StopTime(
        "OCESN862418F0100545013",
        misc.DateTime.parseOrFail("2016-02-05T18:42:06.232+01:00"),
        misc.DateTime.parseOrFail("2016-02-05T18:53:06.232+01:00"),
        "8739098",
        2
      ),
      StopTime(
        "OCESN862418F0100545013",
        misc.DateTime.parseOrFail("2016-02-05T19:42:06.232+01:00"),
        misc.DateTime.parseOrFail("2016-02-05T19:53:06.232+01:00"),
        "8739100",
        3
      )
    )
  )

  val tripB = Trip(
    "tripB",
    "xxx",
    Calendar.off,
    None,
    List(
      StopTime(
        "OCESN862418F0100545013B",
        misc.DateTime.parseOrFail("2016-02-05T17:02:06.232+01:00"),
        misc.DateTime.parseOrFail("2016-02-05T17:02:06.232+01:00"),
        "8739400",
        0
      ),
      StopTime(
        "OCESN862418F0100545013B",
        misc.DateTime.parseOrFail("2016-02-05T17:42:06.232+01:00"),
        misc.DateTime.parseOrFail("2016-02-05T17:53:06.232+01:00"),
        "8739099",
        1
      ),
      StopTime(
        "OCESN862418F0100545013B",
        misc.DateTime.parseOrFail("2016-02-05T18:42:06.232+01:00"),
        misc.DateTime.parseOrFail("2016-02-05T18:53:06.232+01:00"),
        "8739098",
        2
      ),
      StopTime(
        "OCESN862418F0100545013B",
        misc.DateTime.parseOrFail("2016-02-05T19:42:06.232+01:00"),
        misc.DateTime.parseOrFail("2016-02-05T19:53:06.232+01:00"),
        "8739100",
        3
      )
    )
  )

  val tripC = Trip(
    "tripC",
    "xxx",
    Calendar.off,
    None,
    List(
      StopTime(
        "OCESN862418F0100545013B",
        misc.DateTime.parseOrFail("2016-02-05T17:42:06.232+01:00"),
        misc.DateTime.parseOrFail("2016-02-05T17:53:06.232+01:00"),
        "8739099",
        1
      ),
      StopTime(
        "OCESN862418F0100545013B",
        misc.DateTime.parseOrFail("2016-02-05T18:42:06.232+01:00"),
        misc.DateTime.parseOrFail("2016-02-05T18:53:06.232+01:00"),
        "8739098",
        2
      )
    )
  )
}
