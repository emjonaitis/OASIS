#  percent
#' Internal function for printing two values in the format required for OASIS course evaluation reports.
#' The first value is unmarked; the second, on a newline, is smaller and italicized.
#' Used in courseEval.
#' 
#' @param var1 Variable containing current year data (for unmarked inclusion).
#' @param var2 Variable containing past year data (for marked inclusion).
#' @param syntax Variable denoting whether LaTeX ("latex") or RMarkdown ("md") syntax is desired. Defaults to "md".
#' 
#' @export
#' @return Character vector combining the two values with formatting markup, including a hard line break between.
#' 
#' @examples
#' N1 <- 100
#' N2 <- 89
#' twoline(N1,N2,"latex")
#' twoline(N1,N2,"md")
#' 
#' @seealso \code{\link{courseEval}}
#' @keywords OASIS, course evaluation, knitr, RMarkdown, LaTeX

twoline <- function(var1,var2,syntax) {
  if (syntax == "latex") {
    start <- "{\\large"
    mid   <- "}\n\\textit{\\small"
    end   <- "}"
  }
  else if (syntax == "md") {
    start <- ""
    mid   <- "\\\n*^"
    end   <- "^*"
  }
  else {
    stop("Syntax must be one of: \"latex\", \"md\"")
  }
  
  var12 <- paste0(start,var1,mid,var2,end)
  return (var12)
}