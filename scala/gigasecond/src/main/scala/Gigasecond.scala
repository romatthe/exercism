import java.time.LocalDate
import java.time.LocalDateTime
import java.time.temporal.{ChronoUnit, TemporalUnit}

object Gigasecond {
  def add(startDate: LocalDate): LocalDateTime = {
    startDate.atStartOfDay.plus(1000000000, ChronoUnit.SECONDS)
  }

  def add(startDateTime: LocalDateTime): LocalDateTime = {
    startDateTime.plus(1000000000, ChronoUnit.SECONDS)
  }
}
