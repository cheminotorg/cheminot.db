package m.cheminot.build.tests

import m.cheminot.build._

object Main {

  def main(args: Array[String]): Unit = {
    BuilderTests.testDistinctTrips()
  }
}

object BuilderTests {

  def testDistinctTrips(): Unit = {
    import org.joda.time.format.ISODateTimeFormat

    val tripA = Trip(
      "tripA",
      "xxx",
      None,
      None,
      List(
        StopTime(
          "OCESN862418F0100545013",
          ISODateTimeFormat.dateTime.parseDateTime("2016-02-05T17:02:06.232+01:00"),
          ISODateTimeFormat.dateTime.parseDateTime("2016-02-05T17:02:06.232+01:00"),
          "8739400",

          0
        ),
        StopTime(
          "OCESN862418F0100545013",
          ISODateTimeFormat.dateTime.parseDateTime("2016-02-05T17:42:06.232+01:00"),
          ISODateTimeFormat.dateTime.parseDateTime("2016-02-05T17:53:06.232+01:00"),
          "8739099",
          1
        ),
        StopTime(
          "OCESN862418F0100545013",
          ISODateTimeFormat.dateTime.parseDateTime("2016-02-05T18:42:06.232+01:00"),
          ISODateTimeFormat.dateTime.parseDateTime("2016-02-05T18:53:06.232+01:00"),
          "8739098",
          2
        ),
        StopTime(
          "OCESN862418F0100545013",
          ISODateTimeFormat.dateTime.parseDateTime("2016-02-05T19:42:06.232+01:00"),
          ISODateTimeFormat.dateTime.parseDateTime("2016-02-05T19:53:06.232+01:00"),
          "8739100",
          3
        )
      )
    )

    val tripB = Trip(
      "tripB",
      "xxx",
      None,
      None,
      List(
        StopTime(
          "OCESN862418F0100545013B",
          ISODateTimeFormat.dateTime.parseDateTime("2016-02-05T17:02:06.232+01:00"),
          ISODateTimeFormat.dateTime.parseDateTime("2016-02-05T17:02:06.232+01:00"),
          "8739400",
          0
        ),
        StopTime(
          "OCESN862418F0100545013B",
          ISODateTimeFormat.dateTime.parseDateTime("2016-02-05T17:42:06.232+01:00"),
          ISODateTimeFormat.dateTime.parseDateTime("2016-02-05T17:53:06.232+01:00"),
          "8739099",
          1
        ),
        StopTime(
          "OCESN862418F0100545013B",
          ISODateTimeFormat.dateTime.parseDateTime("2016-02-05T18:42:06.232+01:00"),
          ISODateTimeFormat.dateTime.parseDateTime("2016-02-05T18:53:06.232+01:00"),
          "8739098",
          2
        ),
        StopTime(
          "OCESN862418F0100545013B",
          ISODateTimeFormat.dateTime.parseDateTime("2016-02-05T19:42:06.232+01:00"),
          ISODateTimeFormat.dateTime.parseDateTime("2016-02-05T19:53:06.232+01:00"),
          "8739100",
          3
        )
      )
    )

    val tripC = Trip(
      "tripC",
      "xxx",
      None,
      None,
      List(
        StopTime(
          "OCESN862418F0100545013B",
          ISODateTimeFormat.dateTime.parseDateTime("2016-02-05T17:42:06.232+01:00"),
          ISODateTimeFormat.dateTime.parseDateTime("2016-02-05T17:53:06.232+01:00"),
          "8739099",
          1
        ),
        StopTime(
          "OCESN862418F0100545013B",
          ISODateTimeFormat.dateTime.parseDateTime("2016-02-05T18:42:06.232+01:00"),
          ISODateTimeFormat.dateTime.parseDateTime("2016-02-05T18:53:06.232+01:00"),
          "8739098",
          2
        )
      )
    )

    println(Builder.distinctTrips(List(tripA, tripB)).map(_.id))
    println(Builder.distinctTrips(List(tripA, tripB, tripC)).map(_.id))
    println(Builder.distinctTrips(List(tripA, tripC, tripB)).map(_.id))
  }
}
