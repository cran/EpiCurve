\name{EpiCurve}
\alias{EpiCurve}

\title{Plot an Epidemic Curve}
\description{
This function plot an epidemic curve with ggplot2}
\usage{
EpiCurve(x, date = NULL,freq = NULL, cutvar = NULL,
   period = NULL, split = 1, cutorder = NULL, colors = NULL,
   title = NULL, xlabel = NULL, ylabel=NULL, note=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data.frame with at least one column with Date type}
  \item{date}{character, name of Date column}
  \item{freq}{character, name of a column with a value to display}
  \item{cutvar}{character, name of a column with factors}
  \item{period}{character, c("hour", "day","week", "month")}
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
# library(ISOweek)
#
# DF <- read.csv("daily_cases.csv", stringsAsFactors=FALSE)
# DF <- DF[1, ]
# DF$date <- as.Date(DF$date)
# DF_V <- DF[DF[,"factor"] == "Valid", 1:2]
# DF_I <- DF[DF[,"factor"] == "Invalid", 1:2]
# EpiCurve(DF,
#    date = "date",
#    freq = "value",
#    cutvar = "factor",
#    period = "day",
#    cutorder = c("Invalid", "Valid"),
#    color=c("#009900", "#000099"),
#    ylabel="Number of cases",
#    xlabel=sprintf("Du %s au %s", min(DF$date), max(DF$date)),
#    title = "Epidemic Curve\n",
#    note = "This is the Epidemic Curve of cases from 2016-03-01 to 2016-06-29")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
