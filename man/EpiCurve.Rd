\name{EpiCurve}
\alias{EpiCurve}

\title{Plot an Epidemic Curve}
\description{
This function plot an epidemic curve with ggplot2}
\usage{
EpiCurve(x, date = NULL,freq = NULL, cutvar = NULL,
   period = NULL, to.period = NULL, split = 1, cutorder = NULL, colors = NULL,
   title = NULL, xlabel = NULL, ylabel=NULL, note=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data.frame with at least one column with Date type}
  \item{date}{character, name of Date column}
  \item{freq}{character, name of a column with a value to display}
  \item{cutvar}{character, name of a column with factors}
  \item{period}{character, c("hour", "day","week", "month")}
  \item{to.period}{character, Convert date period to another period only for aggregated data. If period is "day", to.period can be "week" or "month". If period is "week", to.period can be "month".}
  \item{split}{integer, c(1,2,3,4,6,8,12) Value for hourly split}
  \item{cutorder}{character vector of factors}
  \item{colors}{character vector of colors}
  \item{title}{character, title of the plot}
  \item{xlabel}{character, label for x axis}
  \item{ylabel}{character, label for y axis}
  \item{note}{character, add a note under the graph}
}

\details{
When period is "week" the date MUST be in ISOweek format YYYY-WNN and library ISOweek is needed. When period is "month" the date MUST be formated YYYY-MM.

When period is "hour" the date MUST be in timeDate format (YYYY-mm-dd HH:MM:SS) or (YYYY-mm-dd HH:MM)

}

\references{
  <https://rlab-epiconcept.blogspot.fr/2016/09/courbes-epidemiques-avec-ggplot2.html>

  <https://fr.wikipedia.org/wiki/Courbe_épidémique>

}
\author{
  <jp.decorps@epiconcept.fr> & <m.vallee@epiconcept.fr>
}

\examples{
# library(Epicurve)
date <- seq(as.timeDate("2017-05-10 21:35:22"), as.timeDate("2017-05-12 06:15:12"), by="12 min")
val <- rep(1, length(date))
tri <- rep(c("Alive", "Died","Unknown"), length.out=length(date))
DF <- data.frame(date, val, tri)
names(DF) <- c("date","value", "tri")


EpiCurve(DF,
         date = "date",
         freq = "value",
         period = "hour",
         split = 4,
         cutvar = "tri",
         ylabel="Number of cases",
         xlabel= "From 2017-05-10 21:35:22 To 2017-05-12 06:15:12",
         title = "Epidemic Curve")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~documentation }
