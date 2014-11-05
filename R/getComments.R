#  courseEval
#' Function for taking in a raw course evaluation dataset from OASIS
#' and outputting a table of interim values that can be later formatted for use
#' with knitr/RMarkdown/LaTeX.
#' The end result becomes a pdf suitable for the final course evaluation report.
#' 
#' @param data Data frame containing the raw data.
#' 
#' @export
#' @return List of data frames containing only the comments.
#' 
#' @examples
#' df <- data(sample)
#' foo <- getComments(df)
#' foo
#' 
#' @seealso \code{\link{courseEval}}
#' @keywords OASIS, course evaluation, knitr
 
getComments <- function(data)
{
  
  # Need to do some error checking here so that if data format changes, we aren't caught unawares.
  
  library(lubridate)
  library(plyr)
  library(jonaitis)
  data$Year <- with(data,
                    year(parse_date_time(Start.Date,orders=c("ymd","mdy"))))
  firstline <- data[1,]
  filename <- with(firstline,paste(Course.ID,Course,"comments",sep=" - "))
  
  datalist <- list(filename)
  keep <- with(subset(data,Question.Number>=11& Year==max(Year)),
               data.frame(Question.Number,Answer.text,stringsAsFactors=FALSE))
  
  colnames(keep) <- c("QuestionNo","Answer")
  
  # Summarize
  for (i in 12:15) {
    subdata <- subset(keep,QuestionNo==i & nchar(Answer)>1)
    subdata <- with(subdata,data.frame(code=rep("",length(Answer)),comment=Answer,stringsAsFactors=FALSE))
    datalist[[i-10]] <- subdata
  }
  return(datalist)
}