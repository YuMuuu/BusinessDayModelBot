package BusinessDayModelBot


import scalikejdbc._
import businesscalendarparser.BusinessCalendarParser
import businesscalendarparser.Token
import java.time.LocalDate
import businesscalendarparser.Token.Center
import businesscalendarparser.Token.EN
import businesscalendarparser.Token.JP
import businesscalendarparser.Token.JPAndEN
import businesscalendarparser.Token.JPOrEn
import scala.util.Try
import java.time.format.DateTimeFormatter

object Main extends App {
  Class.forName("org.postgresql.Driver")
  ConnectionPool.singleton("jdbc:postgresql://localhost:5432/postgres", "root", "root")
  
  implicit val session = AutoSession

  val input = "2020/12/22^jp+1"
  println("input: " + input)

  val token = BusinessCalendarParser(input)
  println("token: " + token)

  def lexer(expr: Token.Expr): Option[Token.Calendar] = {
      expr match {
          case b: Token.BusinessDayCalendar => {
              b.expr match {
                  case bb: Token.BusinessDayCalendar => lexer(b.expr)
                  case cc: Token.Calendar => {
                    hjpPlus(cc, b.maybeInt.get)
                  }
              }
          }
          case c: Token.Calendar => Some(c)
      }
  }
  val result = hjpPlus(Token.Calendar(LocalDate.of(2020, 12, 22)), Token.Num(1))
  println("result: " + result.map(_.localDate))

    



  def toLocalDate(str: String): Option[LocalDate] = {
    Try( LocalDate.parse(str, DateTimeFormatter.ofPattern("yyyy-MM-dd"))).toOption
  }

    
    //  hjp
  def hjp(calendar: Token.Calendar)(implicit s: DBSession = AutoSession): Option[Token.Calendar] = 
    for {
      str <- sql"SELECT date FROM jp_businessdate WHERE date >= ${calendar.localDate} ORDER BY date ASC LIMIT 1;".map(rs => rs.string("date")).single.apply()
      localDate <- toLocalDate(str)
    } yield Token.Calendar(localDate)
  
  //  hjp +
  def hjpPlus(calendar: Token.Calendar, num: Token.Num)(implicit s: DBSession = AutoSession): Option[Token.Calendar] = 
    for {
      str <- sql"SELECT date FROM jp_businessdate WHERE date >= ${calendar.localDate} ORDER BY date ASC OFFSET ${num.int} LIMIT 1;".map(rs => rs.string("date")).single.apply()
      localDate <- toLocalDate(str)
    } yield Token.Calendar(localDate)
    
  //  hjp -
  def hjpMinus(calendar: Token.Calendar, num: Token.Num)(implicit s: DBSession = AutoSession): Option[Token.Calendar] = 
    for {
      c <- hjp(calendar)  
      str <- sql"SELECT date FROM jp_businessdate WHERE date <= ${c.localDate} ORDER BY date DESC OFFSET ${num.int} LIMIT 1;".map(rs => rs.string("date")).single.apply()
      localDate <- toLocalDate(str)
    } yield Token.Calendar(localDate)

  //  hen
  def hen(calendar: Token.Calendar): Option[Token.Calendar] = 
    for {
      str <- sql"SELECT date FROM en_businessdate WHERE date >= ${calendar.localDate} ORDER BY date ASC LIMIT 1;".map(rs => rs.string("date")).single.apply()
      localDate <- toLocalDate(str)
    } yield Token.Calendar(localDate)
  //  hen +
  def henPlus(calendar: Token.Calendar, num: Token.Num): Option[Token.Calendar] = 
    for {
      str <- sql"SELECT date FROM en_businessdate WHERE date >= ${calendar.localDate} ORDER BY date ASC OFFSET ${num.int} LIMIT 1;".map(rs => rs.string("date")).single.apply()
      localDate <- toLocalDate(str)
    } yield Token.Calendar(localDate)
  //  hen -
  def henMinus(calendar: Token.Calendar, num: Token.Num): Option[Token.Calendar] = 
    for {
      c <- hen(calendar)  
      str <- sql"SELECT date FROM en_businessdate WHERE date <= ${c.localDate} ORDER BY date DESC OFFSET ${num.int} LIMIT 1;".map(rs => rs.string("date")).single.apply()
      localDate <- toLocalDate(str)
    } yield Token.Calendar(localDate)

  //  hc
  def hc(calendar: Token.Calendar): Option[Token.Calendar] = Some(calendar)
  //  hc +
  def hcenterPlus(calendar: Token.Calendar, num: Token.Num): Option[Token.Calendar] =  Some(Token.Calendar(calendar.localDate.plusDays(num.int)))
  //  hc -
  def hcenterMinus(calendar: Token.Calendar, num: Token.Num): Option[Token.Calendar] =  Some(Token.Calendar(calendar.localDate.minusDays(num.int)))
  
  //  hjp&en
  def hjpAndEn(calendar: Token.Calendar): Option[Token.Calendar] =     
    for {
      str <- sql"SELECT * FROM jp_businessdate INNER JOIN en_businessdate ON jp_businessdate.date = en_businessdate.date  WHERE jp_businessdate.date >= ${calendar.localDate} ORDER BY jp_businessdate.date ASC LIMIT 1;".map(rs => rs.string("date")).single.apply()
      localDate <- toLocalDate(str)
    } yield Token.Calendar(localDate)
  //  hjp&en +
  def hjpAndEnPlus(calendar: Token.Calendar, num: Token.Num): Option[Token.Calendar] = 
    for {
      str <- sql"SELECT * FROM jp_businessdate INNER JOIN en_businessdate ON jp_businessdate.date = en_businessdate.date  WHERE jp_businessdate.date >= ${calendar.localDate} ORDER BY jp_businessdate.date ASC OFFSET ${num.int} LIMIT 1;"
             .map(rs => rs.string("date")).single.apply()
      localDate <- toLocalDate(str)
    } yield Token.Calendar(localDate)
  //  hjp&en -
  def hjpAndEnMinus(calendar: Token.Calendar, num: Token.Num): Option[Token.Calendar] = 
    for {
      c <- hjpAndEn(calendar)  
      str <- sql"SELECT * FROM jp_businessdate INNER JOIN en_businessdate ON jp_businessdate.date = en_businessdate.date  WHERE jp_businessdate.date <= ${c.localDate} ORDER BY jp_businessdate.date DESC OFFSET ${num.int} LIMIT 1;"
             .map(rs => rs.string("date")).single.apply()
      localDate <- toLocalDate(str)
    } yield Token.Calendar(localDate)

  //  hjp|en
  def hjpOrEn(calendar: Token.Calendar): Option[Token.Calendar] = 
    for {
      str <- sql"(SELECT * FROM jp_businessdate) UNION (SELECT * FROM en_businessdate) WHERE jp_businessdate.date >= ${calendar.localDate} ORDER BY jp_businessdate.date ASC LIMIT 1;".map(rs => rs.string("date")).single.apply()
      localDate <- toLocalDate(str)
    } yield Token.Calendar(localDate)
    
  //  hjp|en +
  def hjpOrEnPlus(calendar: Token.Calendar, num: Token.Num): Option[Token.Calendar] = 
    for {
      str <- sql"(SELECT * FROM jp_businessdate) UNION (SELECT * FROM en_businessdate) WHERE jp_businessdate.date >= ${calendar.localDate} ORDER BY jp_businessdate.date ASC OFFSET ${num.int} LIMIT 1;".map(rs => rs.string("date")).single.apply()
      localDate <- toLocalDate(str)
    } yield Token.Calendar(localDate)
  //  hjp|en -
  def hjpOrEnMinus(calendar: Token.Calendar, num: Token.Num): Option[Token.Calendar] = 
    for {
      c <- hjpOrEn(calendar)  
      str <- sql"(SELECT * FROM jp_businessdate) UNION (SELECT * FROM en_businessdate)  WHERE jp_businessdate.date <= ${c.localDate} ORDER BY jp_businessdate.date DESC OFFSET ${num.int} LIMIT 1;"
             .map(rs => rs.string("date")).single.apply()
      localDate <- toLocalDate(str)
    } yield Token.Calendar(localDate)


  //  ujp
  def ujp(calendar: Token.Calendar): Option[Token.Calendar] = ???
  //  ujp +
  def ujpPlus(calendar: Token.Calendar, num: Token.Num): Option[Token.Calendar] = ???
  //  ujp -
  def ujpMinus(calendar: Token.Calendar, num: Token.Num): Option[Token.Calendar] = ???

  //  uen
  def uen(calendar: Token.Calendar): Option[Token.Calendar] = ???
  //  uen +
  def uenPlus(calendar: Token.Calendar, num: Token.Num): Option[Token.Calendar] = ???
  //  uen -
  def uenMinus(calendar: Token.Calendar, num: Token.Num): Option[Token.Calendar] = ???

  //  uc
  def uc(calendar: Token.Calendar): Option[Token.Calendar] = Some(calendar)
  //  uc +
  def ucenterPlus(calendar: Token.Calendar, num: Token.Num): Option[Token.Calendar] = Some(Token.Calendar(calendar.localDate.plusDays(num.int)))
  //  uc -
  def ucenterMinus(calendar: Token.Calendar, num: Token.Num): Option[Token.Calendar] = Some(Token.Calendar(calendar.localDate.minusDays(num.int)))

  //  ujp&en
  def ujpAndEn(calendar: Token.Calendar): Option[Token.Calendar] = ???
  //  ujp&*en +
  def ujpAndEnPlus(calendar: Token.Calendar, num: Token.Num): Option[Token.Calendar] = ???
  //  ujp&*en -
  def ujpAndEnMinus(calendar: Token.Calendar, num: Token.Num): Option[Token.Calendar] = ???

  //  ujp|en
  def ujpOrEn(calendar: Token.Calendar): Option[Token.Calendar] = ???
  //  ujp|en +
  def ujpOrEnPlus(calendar: Token.Calendar, num: Token.Num): Option[Token.Calendar] = ???
  //  ujp|en -
  def ujpOrEnMinus(calendar: Token.Calendar, num: Token.Num): Option[Token.Calendar] = ???
}