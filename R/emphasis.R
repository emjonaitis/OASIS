#  emphasis
#' Internal function for emphasizing certain words in the row labels for OASIS course evaluation reports.
#' Used in courseEval.
#' 
#' @param strings Array of strings containing words to be emphasized.
#' @param words List of words or phrases to emphasize within each string.
#' @param syntax Variable denoting whether LaTeX ("latex") or RMarkdown ("md") syntax is desired. Defaults to "md".
#' 
#' @export
#' @return Character vector combining the two values with formatting markup, including a hard line break between.
#' 
#' @examples
#' strings <- c("I hope you have a nice day","I'll see you next week")
#' words <- list(c("hope","nice"),"you")
#' emphasis(strings,words,syntax="md")
#' 
#' @seealso \code{\link{courseEval}}
#' @keywords OASIS, course evaluation, knitr, RMarkdown, LaTeX

emphasis <- function(strings,words,syntax="md") {
  if (length(strings) != length(words)) {
    stop("Number of list items must equal length of string array")
  }
  
  if (syntax == "latex") {
    start <- "\\\textbf{"
    end   <- "}"
  }
  else if (syntax == "md") {
    start <- "**"
    end   <- "**"
  }
  else {
    stop("Syntax must be one of: \"latex\", \"md\"")
  }
  
  for (foo in 1:length(strings)) {
    for (bar in 1:length(words[[foo]])) {
      strings[foo] <- gsub(words[[foo]][bar],paste0(start,words[[foo]][bar],end),strings[foo])
    }
  }
  return (strings)
}