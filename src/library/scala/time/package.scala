package scala

import javax.time._

package object time {
  type TimeUnit = java.util.concurrent.TimeUnit

  final val NanosPerSecond = 1000000000
  def Zero = Duration.ZERO
  def now  = Instant.now

  implicit def mkLongDurationOps(amount: Long): LongDurationOps = new LongDurationOps(amount)
  implicit def mkDurationOps(duration: Duration): DurationOps   = new DurationOps(duration)
  implicit def mkInstantOps(instant: Instant): InstantOps       = new InstantOps(instant)

  final class LongDurationOps(val amount: Long) extends AnyVal {
    def nanos              = Duration.ofNanos(amount)
    def micros             = Duration.ofNanos(amount * 1000)
    def millis             = Duration.ofMillis(amount)
    def seconds            = Duration.ofSeconds(amount)
    def minutes            = Duration.ofMinutes(amount)
    def hours              = Duration.ofHours(amount)
    def days               = Duration.ofDays(amount)
    def of(unit: TimeUnit) = Duration.of(amount, unit)
  }
  final class DurationOps(val duration: Duration) extends AnyVal {
    def unary_-                = duration.negated()
    def + (other: Duration)    = duration plus other
    def - (other: Duration)    = duration minus other
    def * (multiplicand: Long) = duration multipliedBy multiplicand
    def / (divisor: Long)      = duration dividedBy divisor
    def < (other: Duration)    = duration isLessThan other
    def > (other: Duration)    = duration isGreaterThan other
  }
  final class InstantOps(val instant: Instant) extends AnyVal {
    def + (other: Duration)    = instant plus other
    def - (other: Duration)    = instant minus other
    def < (other: Instant)     = instant isBefore other
    def > (other: Instant)     = instant isAfter other
  }
}
