\name{fetchFX}
\alias{fetchFX}
\title{
  Download historical FX data
}
\description{
  Download historical FX data from the PACIFIC Exchange Rate
  Service, run by Werner Antweiler at Sauder School of Business,
  University of British Columbia. Visit the homepage
  \url{http://fx.sauder.ubc.ca/} to learn more.
}
\usage{
fetchFX(baseCurrency = "USD", targetCurrencies = c("EUR", "AUD"),
        startDate = "2010-01-01",endDate,
        dataFrequency = "daily", notation = "volume")
}
\arguments{
  \item{baseCurrency}{
    base currency (eg, \code{"EUR"})
}
  \item{targetCurrencies}{
    target currency; can be a vector like \code{c("CHF", "USD")}
}
  \item{startDate}{
    a length-one vector of class \code{Date}, or a length-one vector of
    class \code{character} in \acronym{ISO} 8601 format (ie,
    \acronym{YYYY-MM-DD})
}
  \item{endDate}{
    a length-one vector of class \code{Date}, or a length-one vector of
    class \code{character} in \acronym{ISO} 8601 format (ie,
    \acronym{YYYY-MM-DD}). If missing, the current date is used.
}
  \item{dataFrequency}{a length-one character vector: either
    \code{"daily"}, \code{"weekly"} or \code{"monthly"}
  }
  \item{notation}{
    a length-one character vector: either \code{"volume"} or \code{"price"}}
}
\details{
  \emph{The database is for academic purposes only.} Please see the
  \acronym{FAQ} at \url{http://fx.sauder.ubc.ca/FAQ.html} for the terms of use.
  
  There are two types of quoting convention, set through the argument
  \code{notation}. Volume notation: how many units of the
  \code{targetCurrency} do I have to give for one unit of the
  \code{baseCurrency}? Price notation: how many units of the
  \code{baseCurrency} do I have to give for one unit of the
  \code{targetCurrency}?
    
  The web interface to the database restricts daily and weekly data to
  no more four calendar years per download. \code{fetchFX} will
  automatically loop in such cases (but will add a delay of one second
  in each iteration).
}
\note{
  In the FX markets, currencies are typically quoted as \code{FX1FX2},
  which reads \sQuote{How many units of \code{FX2} do I have to give to
  get one unit of \code{FX1}?} For instance, \code{EURUSD} means
  \sQuote{how many dollars do I have to give for one euro?} This is
  \strong{not} how currencies are labelled in the PACIFIC Exchange Rate
  Service.
}
\value{

  The function returns a data.frame. Column \code{"Jul.Dates"} contains
  the Julian dates (see \url{http://fx.sauder.ubc.ca/julian.html}) in
  \code{numeric} form; column \code{"Dates"} the calendar dates (class
  \code{Date}); the following columns contain the exchange rates.
  
}
\author{
Enrico Schumann
  
Maintainer: Enrico Schumann <es@enricoschumann.net>
}
\examples{
\dontrun{
x <- fetchFX("USD", targetCurrencies = c("EUR", "AUD"),
             startDate = as.Date("2006-01-01"),
             endDate   = as.Date("2011-10-01"),
             dataFrequency = "weekly",
             notation = "price")
head(x, 3L)
tail(x, 3L)}
}
