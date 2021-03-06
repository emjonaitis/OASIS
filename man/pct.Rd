% Generated by roxygen2 (4.0.0): do not edit by hand
\name{pct}
\alias{pct}
\title{Internal function for formatting a proportion as a percent in either
Markdown or LaTeX. Used in courseEval.}
\usage{
pct(var, syntax = "md")
}
\arguments{
\item{var}{Variable containing the value to be formatted. Must be a proportion (between 0 and 1 inclusive).}

\item{Variable}{denoting whether LaTeX ("latex") or RMarkdown ("md") syntax is desired. Defaults to "md".}
}
\value{
Character vector combining the value of \code{var}, times 100, rounded to 1 decimal place, and the symbol '%'.
}
\description{
Internal function for formatting a proportion as a percent in either
Markdown or LaTeX. Used in courseEval.
}
\examples{
x <- ceiling(runif(1,0,100))
pct(x,"md")
pct(x,"latex")
}
\seealso{
\code{\link{courseEval}}, \code{\link{pandoc.table}}
}
\keyword{LaTeX}
\keyword{OASIS,}
\keyword{RMarkdown,}
\keyword{course}
\keyword{evaluation,}
\keyword{knitr,}

