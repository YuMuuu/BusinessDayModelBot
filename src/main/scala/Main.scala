package BusinessDayModelBot

import scalikejdbc._
import scalikejdbc.config._
import businesscalendarparser.BusinessCalendarParser
import businesscalendarparser.Token._
import java.time.LocalDate
import scala.util.Try
import java.time.format.DateTimeFormatter
import org.slf4j.Logger;
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.LazyLogging

object Main extends App with LazyLogging {
  // scalikejdbc setting
  Class.forName("org.postgresql.Driver")
  ConnectionPool.singleton(
    "jdbc:postgresql://localhost:5432/postgres",
    "root",
    "root"
  )
  GlobalSettings.loggingSQLAndTime = LoggingSQLAndTimeSettings(
    enabled = false,
    logLevel = "DEBUG",
    warningEnabled = true,
    warningThresholdMillis = 1000L,
    warningLogLevel = "WARN"
  )

  implicit val session = AutoSession

  val input = args(0)
  logger.info("input: " + input)

  val token = BusinessCalendarParser(input)
  logger.info("token: " + token)

  val result: Option[Expr] = token.toOption.flatMap(lexer)
  val afterResult = result match {
    case c: Option[Calendar] => c.map(_.localDate)
    case _                   => None
  }
  logger.info("result: " + afterResult)

  // one step eval
  def lexer(expr: Expr): Option[Expr] = {
    logger.debug("lexer arg:" + expr)

    expr match {
      case bb @ BusinessDayCalendar(
            b: BusinessDayCalendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) => {
        for {
          c <- lexer(b)
          bc = BusinessDayCalendar(c, castOp, cal, maybeBinOp, maybeInt)
          cc <- run(bc)
        } yield cc
      }
      case bc @ BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) =>
        run(bc)
    }
  }

  def run(bc: Expr): Option[Calendar] = {
    logger.debug("run arg: " + bc)
    //  && maybeInt == Some(Num)
    // 上記のif 式でのfilterがうまくいかない？ maybeBinOpでパターンは網羅できるので暫定対応で消す

    bc match {
      //  hjp
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == Hat && cal == JP && maybeBinOp == None =>
        hjp(c)
      //  hjp +
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == Hat && cal == JP && maybeBinOp == Some(Plus) =>
        hjpPlus(c, maybeInt.get)
      //  hjp -
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == Hat && cal == JP && maybeBinOp == Some(Minus) =>
        hjpMinus(c, maybeInt.get)
      //  hen
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == Hat && cal == EN && maybeBinOp == None =>
        hen(c)
      //  hen +
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == Hat && cal == EN && maybeBinOp == Some(Plus) =>
        henPlus(c, maybeInt.get)
      //  hen -
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == Hat && cal == EN && maybeBinOp == Some(Minus) =>
        henMinus(c, maybeInt.get)
      //  hc
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == Hat && cal == Center && maybeBinOp == None =>
        hc(c)
      //  hc +
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == Hat && cal == Center && maybeBinOp == Some(Plus) =>
        hcenterPlus(c, maybeInt.get)
      //  hc -
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == Hat && cal == Center && maybeBinOp == Some(Minus) =>
        hcenterMinus(c, maybeInt.get)
      //  hjp&en
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == Hat && cal == JPAndEN && maybeBinOp == None =>
        hjpAndEn(c)
      //  hjp&en +
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == Hat && cal == JPAndEN && maybeBinOp == Some(Plus) =>
        hjpAndEnPlus(c, maybeInt.get)
      //  hjp&en -
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == Hat && cal == JPAndEN && maybeBinOp == Some(Minus) =>
        hjpAndEnMinus(c, maybeInt.get)
      //  hjp|en
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == Hat && cal == JPOrEN && maybeBinOp == None =>
        hjpOrEn(c)
      //  hjp|en +
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == Hat && cal == JPOrEN && maybeBinOp == Some(Plus) =>
        hjpOrEnPlus(c, maybeInt.get)
      //  hjp|en -
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == Hat && cal == JPOrEN && maybeBinOp == Some(Minus) =>
        hjpOrEnMinus(c, maybeInt.get)
      //  ujp
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == UnderBar && cal == JP && maybeBinOp == None =>
        ujp(c)
      //  ujp +
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == UnderBar && cal == JP && maybeBinOp == Some(Plus) =>
        ujpPlus(c, maybeInt.get)
      //  ujp -
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == UnderBar && cal == JP && maybeBinOp == Some(Minus) =>
        ujpMinus(c, maybeInt.get)
      //  uen
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == UnderBar && cal == EN && maybeBinOp == None =>
        uen(c)
      //  uen +
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == UnderBar && cal == EN && maybeBinOp == Some(Plus) =>
        uenPlus(c, maybeInt.get)
      //  uen -
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == UnderBar && cal == EN && maybeBinOp == Some(Minus) =>
        uenMinus(c, maybeInt.get)
      //  uc
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == UnderBar && cal == Center && maybeBinOp == None =>
        uc(c)
      //  uc +
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          )
          if castOp == UnderBar && cal == Center && maybeBinOp == Some(Plus) =>
        ucenterPlus(c, maybeInt.get)
      //  uc -
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          )
          if castOp == UnderBar && cal == Center && maybeBinOp == Some(Minus) =>
        ucenterMinus(c, maybeInt.get)
      //  ujp&en
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == UnderBar && cal == JPAndEN && maybeBinOp == None =>
        ujpAndEn(c)
      //  ujp&en +
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          )
          if castOp == UnderBar && cal == JPAndEN && maybeBinOp == Some(Plus) =>
        ujpAndEnPlus(c, maybeInt.get)
      //  ujp&en -
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          )
          if castOp == UnderBar && cal == JPAndEN && maybeBinOp == Some(
            Minus
          ) =>
        ujpAndEnMinus(c, maybeInt.get)
      //  ujp|en
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          ) if castOp == UnderBar && cal == JPOrEN && maybeBinOp == None =>
        ujpOrEn(c)
      //  ujp|en +
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          )
          if castOp == UnderBar && cal == JPOrEN && maybeBinOp == Some(Plus) =>
        ujpOrEnPlus(c, maybeInt.get)
      //  ujp|en -
      case BusinessDayCalendar(
            c: Calendar,
            castOp: CastOp,
            cal: Cal,
            maybeBinOp: Option[BinOp],
            maybeInt: Option[Num]
          )
          if castOp == UnderBar && cal == JPOrEN && maybeBinOp == Some(Minus) =>
        ujpOrEnMinus(c, maybeInt.get)

      case c: Calendar => Some(c)
      case _           => throw new Exception("match error")
    }
  }

  def toLocalDate(str: String): Option[LocalDate] = {
    Try(
      LocalDate.parse(str, DateTimeFormatter.ofPattern("yyyy-MM-dd"))
    ).toOption
  }

  //  hjp
  def hjp(calendar: Calendar): Option[Calendar] =
    for {
      str <-
        sql"SELECT date FROM jp_businessdate WHERE date <= ${calendar.localDate} ORDER BY date DESC LIMIT 1;"
          .map(rs => rs.string("date"))
          .single
          .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)
  //  hjp +
  def hjpPlus(calendar: Calendar, num: Num): Option[Calendar] =
    for {
      str <-
        sql"SELECT date FROM jp_businessdate WHERE date >= (SELECT date FROM jp_businessdate WHERE date <= ${calendar.localDate} ORDER BY date DESC LIMIT 1) ORDER BY date ASC OFFSET ${num.int} LIMIT 1;"
          .map(rs => rs.string("date"))
          .single
          .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)
  //  hjp -
  def hjpMinus(calendar: Calendar, num: Num): Option[Calendar] =
    for {
      str <-
        sql"SELECT date FROM jp_businessdate WHERE date <= (SELECT date FROM jp_businessdate WHERE date <= ${calendar.localDate} ORDER BY date DESC LIMIT 1) ORDER BY date DESC OFFSET ${num.int} LIMIT 1;"
          .map(rs => rs.string("date"))
          .single
          .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)

  //  hen
  def hen(calendar: Calendar): Option[Calendar] =
    for {
      str <-
        sql"SELECT date FROM en_businessdate WHERE date <= ${calendar.localDate} ORDER BY date DESC LIMIT 1;"
          .map(rs => rs.string("date"))
          .single
          .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)
  //  hen +
  def henPlus(calendar: Calendar, num: Num): Option[Calendar] =
    for {
      str <-
        sql"SELECT date FROM en_businessdate WHERE date >= (SELECT date FROM en_businessdate WHERE date <= ${calendar.localDate} ORDER BY date DESC LIMIT 1) ORDER BY date ASC OFFSET ${num.int} LIMIT 1;"
          .map(rs => rs.string("date"))
          .single
          .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)
  //  hen -
  def henMinus(calendar: Calendar, num: Num): Option[Calendar] =
    for {
      str <-
        sql"SELECT date FROM en_businessdate WHERE date <= (SELECT date FROM en_businessdate WHERE date <= ${calendar.localDate} ORDER BY date DESC LIMIT 1) ORDER BY date DESC OFFSET ${num.int} LIMIT 1;"
          .map(rs => rs.string("date"))
          .single
          .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)

  //  hc
  def hc(calendar: Calendar): Option[Calendar] = Some(calendar)
  //  hc +
  def hcenterPlus(calendar: Calendar, num: Num): Option[Calendar] = Some(
    Calendar(calendar.localDate.plusDays(num.int))
  )
  //  hc -
  def hcenterMinus(calendar: Calendar, num: Num): Option[Calendar] = Some(
    Calendar(calendar.localDate.minusDays(num.int))
  )

  //  hjp&en
  def hjpAndEn(calendar: Calendar): Option[Calendar] =
    for {
      str <-
        sql"""SELECT * FROM jp_businessdate INNER JOIN en_businessdate ON jp_businessdate.date = en_businessdate.date  WHERE jp_businessdate.date <= ${calendar.localDate} ORDER BY jp_businessdate.date DESC LIMIT 1;"""
          .map(rs => rs.string("date"))
          .single
          .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)
  //  hjp&*en +
  def hjpAndEnPlus(calendar: Calendar, num: Num): Option[Calendar] =
    for {
      str <- sql"""
      SELECT date FROM en_businessdate WHERE date >=
      (SELECT * FROM jp_businessdate INNER JOIN en_businessdate ON jp_businessdate.date = en_businessdate.date  WHERE jp_businessdate.date <= ${calendar.localDate} ORDER BY jp_businessdate.date DESC LIMIT 1)
      ORDER BY date ASC OFFSET ${num.int} LIMIT 1
      ;"""
        .map(rs => rs.string("date"))
        .single
        .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)
  //  hjp&*en -
  def hjpAndEnMinus(calendar: Calendar, num: Num): Option[Calendar] =
    for {
      str <- sql"""
      SELECT date FROM en_businessdate WHERE date <=
      (SELECT * FROM jp_businessdate INNER JOIN en_businessdate ON jp_businessdate.date = en_businessdate.date  WHERE jp_businessdate.date <= ${calendar.localDate} ORDER BY jp_businessdate.date DESC LIMIT 1)
      ORDER BY date DESC OFFSET ${num.int} LIMIT 1
      ;"""
        .map(rs => rs.string("date"))
        .single
        .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)
  //  hjp|en
  def hjpOrEn(calendar: Calendar): Option[Calendar] =
    for {
      str <-
        sql"""(SELECT * FROM jp_businessdate) UNION (SELECT * FROM en_businessdate) WHERE jp_businessdate.date <= ${calendar.localDate} ORDER BY jp_businessdate.date DESC LIMIT 1;"""
          .map(rs => rs.string("date"))
          .single
          .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)
  //  hjp|en +
  def hjpOrEnPlus(calendar: Calendar, num: Num): Option[Calendar] =
    for {
      str <- sql"""
      SELECT date FROM en_businessdate WHERE date >=
      ((SELECT * FROM jp_businessdate) UNION (SELECT * FROM en_businessdate) WHERE jp_businessdate.date <= ${calendar.localDate} ORDER BY jp_businessdate.date DESC LIMIT 1)
      ORDER BY date ASC OFFSET ${num.int} LIMIT 1
      ;"""
        .map(rs => rs.string("date"))
        .single
        .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)
  //  hjp|en -
  def hjpOrEnMinus(calendar: Calendar, num: Num): Option[Calendar] = {
    for {
      str <- sql"""
      SELECT date FROM en_businessdate WHERE date <=
      ((SELECT * FROM jp_businessdate) UNION (SELECT * FROM en_businessdate) WHERE jp_businessdate.date <= ${calendar.localDate} ORDER BY jp_businessdate.date DESC LIMIT 1)
      ORDER BY date DESC OFFSET ${num.int} LIMIT 1
      ;"""
        .map(rs => rs.string("date"))
        .single
        .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)
  }

  //  ujp
  def ujp(
      calendar: Calendar
  )(implicit s: DBSession = AutoSession): Option[Calendar] =
    for {
      str <-
        sql"SELECT date FROM jp_businessdate WHERE date >= ${calendar.localDate} ORDER BY date ASC LIMIT 1;"
          .map(rs => rs.string("date"))
          .single
          .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)

  //  ujp +
  def ujpPlus(calendar: Calendar, num: Num)(implicit
      s: DBSession = AutoSession
  ): Option[Calendar] =
    for {
      str <-
        sql"SELECT date FROM jp_businessdate WHERE date >= ${calendar.localDate} ORDER BY date ASC OFFSET ${num.int} LIMIT 1;"
          .map(rs => rs.string("date"))
          .single
          .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)

  //  ujp -
  def ujpMinus(calendar: Calendar, num: Num)(implicit
      s: DBSession = AutoSession
  ): Option[Calendar] =
    for {
      c <- hjp(calendar)
      str <-
        sql"SELECT date FROM jp_businessdate WHERE date <= ${c.localDate} ORDER BY date DESC OFFSET ${num.int} LIMIT 1;"
          .map(rs => rs.string("date"))
          .single
          .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)

  //  uen
  def uen(calendar: Calendar): Option[Calendar] =
    for {
      str <-
        sql"SELECT date FROM en_businessdate WHERE date >= ${calendar.localDate} ORDER BY date ASC LIMIT 1;"
          .map(rs => rs.string("date"))
          .single
          .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)
  //  uen +
  def uenPlus(calendar: Calendar, num: Num): Option[Calendar] =
    for {
      str <-
        sql"SELECT date FROM en_businessdate WHERE date >= ${calendar.localDate} ORDER BY date ASC OFFSET ${num.int} LIMIT 1;"
          .map(rs => rs.string("date"))
          .single
          .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)
  //  uen -
  def uenMinus(calendar: Calendar, num: Num): Option[Calendar] =
    for {
      c <- hen(calendar)
      str <-
        sql"SELECT date FROM en_businessdate WHERE date <= ${c.localDate} ORDER BY date DESC OFFSET ${num.int} LIMIT 1;"
          .map(rs => rs.string("date"))
          .single
          .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)

  //  uc
  def uc(calendar: Calendar): Option[Calendar] = Some(calendar)
  //  uc +
  def ucenterPlus(calendar: Calendar, num: Num): Option[Calendar] = Some(
    Calendar(calendar.localDate.plusDays(num.int))
  )
  //  uc -
  def ucenterMinus(calendar: Calendar, num: Num): Option[Calendar] = Some(
    Calendar(calendar.localDate.minusDays(num.int))
  )

  //  ujp&en
  def ujpAndEn(calendar: Calendar): Option[Calendar] =
    for {
      str <-
        sql"SELECT * FROM jp_businessdate INNER JOIN en_businessdate ON jp_businessdate.date = en_businessdate.date  WHERE jp_businessdate.date >= ${calendar.localDate} ORDER BY jp_businessdate.date ASC LIMIT 1;"
          .map(rs => rs.string("date"))
          .single
          .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)
  //  ujp&en +
  def ujpAndEnPlus(calendar: Calendar, num: Num): Option[Calendar] =
    for {
      str <-
        sql"SELECT * FROM jp_businessdate INNER JOIN en_businessdate ON jp_businessdate.date = en_businessdate.date  WHERE jp_businessdate.date >= ${calendar.localDate} ORDER BY jp_businessdate.date ASC OFFSET ${num.int} LIMIT 1;"
          .map(rs => rs.string("date"))
          .single
          .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)
  //  ujp&en -
  def ujpAndEnMinus(calendar: Calendar, num: Num): Option[Calendar] =
    for {
      c <- hjpAndEn(calendar)
      str <-
        sql"SELECT * FROM jp_businessdate INNER JOIN en_businessdate ON jp_businessdate.date = en_businessdate.date  WHERE jp_businessdate.date <= ${c.localDate} ORDER BY jp_businessdate.date DESC OFFSET ${num.int} LIMIT 1;"
          .map(rs => rs.string("date"))
          .single
          .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)

  //  ujp|en
  def ujpOrEn(calendar: Calendar): Option[Calendar] =
    for {
      str <-
        sql"(SELECT * FROM jp_businessdate) UNION (SELECT * FROM en_businessdate) WHERE jp_businessdate.date >= ${calendar.localDate} ORDER BY jp_businessdate.date ASC LIMIT 1;"
          .map(rs => rs.string("date"))
          .single
          .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)

  //  ujp|en +
  def ujpOrEnPlus(calendar: Calendar, num: Num): Option[Calendar] =
    for {
      str <-
        sql"(SELECT * FROM jp_businessdate) UNION (SELECT * FROM en_businessdate) WHERE jp_businessdate.date >= ${calendar.localDate} ORDER BY jp_businessdate.date ASC OFFSET ${num.int} LIMIT 1;"
          .map(rs => rs.string("date"))
          .single
          .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)
  //  ujp|en -
  def ujpOrEnMinus(calendar: Calendar, num: Num): Option[Calendar] =
    for {
      c <- hjpOrEn(calendar)
      str <-
        sql"(SELECT * FROM jp_businessdate) UNION (SELECT * FROM en_businessdate)  WHERE jp_businessdate.date <= ${c.localDate} ORDER BY jp_businessdate.date DESC OFFSET ${num.int} LIMIT 1;"
          .map(rs => rs.string("date"))
          .single
          .apply()
      localDate <- toLocalDate(str)
    } yield Calendar(localDate)

}
