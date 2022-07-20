package evolutions

import java.time.LocalDate
import java.time.format.DateTimeFormatter

object Main extends App {


  val dtf = DateTimeFormatter.ofPattern("yyyymmdd");
  val dt: LocalDate = LocalDate.of(2020, 1, 1);

  val datelist: Iterator[LocalDate] = Iterator.continually(dt.plusDays(1)).takeWhile(it => it == LocalDate.of(2022, 12, 31))

  println(datelist.toArray.toString())
}