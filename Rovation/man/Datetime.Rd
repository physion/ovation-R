\name{Datetime}
\alias{Datetime}
\title{Constructs an Ovation timestamp from date components.}
\usage{
  Datetime(year, month, day, hour = 0, minute = 0,
    second = 0, millisecond = 0, timezone = character())
}
\arguments{
  \item{year}{year}

  \item{month}{month of year}

  \item{day}{day of month}

  \item{hour}{hour of day (optional; default = 0)}

  \item{minute}{minute of hour (optional; default = 0)}

  \item{second}{second of minute (optional; default = 0)}

  \item{millisecond}{milliseconds of second (optional;
  default = 0)}

  \item{timezone}{time zone (optional; default = local time
  zone)}
}
\value{
  created \code{org.joda.time.DateTime} instance
}
\description{
  Constructs an Ovation timestamp from date components.
}

