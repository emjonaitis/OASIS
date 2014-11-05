#  pct
#' Internal function for formatting a proportion as a percent in either 
#' Markdown or LaTeX. Used in courseEval.
#' 
#' @param var Variable containing the value to be formatted. Must be a proportion (between 0 and 1 inclusive).
#' @param Variable denoting whether LaTeX ("latex") or RMarkdown ("md") syntax is desired. Defaults to "md".
#' 
#' @export
#' @return Character vector combining the value of \code{var}, times 100, rounded to 1 decimal place, and the symbol '%'.
#' 
#' @examples
#' x <- ceiling(runif(1,0,100))
#' pct(x,"md")
#' pct(x,"latex")
#' 
#' @seealso \code{\link{courseEval}}, \code{\link{pandoc.table}}
#' @keywords OASIS, course evaluation, knitr, RMarkdown, LaTeX

pct <- function(var,syntax="md") {
  if (min(var) < 0 | max(var) > 1) {
    stop("Parameter var must be between 0 and 1")
  }
  
  if (syntax == "latex") {
    end <- "\\%"
  }
  else if (syntax == "md") {
    end   <- "%"
  }
  else {
    stop("Syntax must be one of: \"latex\", \"md\"")
  }
  
  out <- paste0(sprintf("%.1f",100*var),end)
  return (out)
}
