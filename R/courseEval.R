#  courseEval
#' Function for taking in a raw course evaluation dataset from OASIS
#' and outputting a table of interim values that can be later formatted for use
#' with knitr/RMarkdown/LaTeX.
#' The end result becomes a pdf suitable for the final course evaluation report.
#' 
#' @param data Data frame containing the raw data.
#' @param syntax Variable denoting whether LaTeX ("latex") or RMarkdown ("md") syntax is desired. Defaults to "md".
#' 
#' @export
#' @return Table suitable for reformatting and printing with pander. Contains
#' Ns, means, and SDs for current and prior year, as well as the percentage of
#' the current year sample responding in each of 7 response categories.
#' 
#' @examples
#' df <- data(sample)
#' foo <- courseEval(df)
#' foo
#' 
#' @seealso \code{\link{welchEMJ}}, \code{\link{pandoc.table}}
#' @keywords OASIS, course evaluation, knitr
 
courseEval <- function(data,syntax="md")
{
  
  # Need to do some error checking here so that if data format changes, we aren't caught unawares.
  
  library(lubridate)
  library(plyr)
  library(jonaitis)
  data$Year <- with(data,
                    year(parse_date_time(Start.Date,orders=c("ymd","mdy"))))
  
  keep <- with(subset(data,Question.Number<=10),
               data.frame(Course.ID,Year,Course,
                          Question.Number,Question,Multiple.Choice.Value,stringsAsFactors=FALSE))
  
  colnames(keep) <- c("CourseID","Year","CourseName",
                      "QuestionNo","Question","Response")
  
  # Summarize
  alldata <- ddply(keep, .(Year,QuestionNo), summarize,
                   Question=Question[1],
                   N = length(Response),
                   StdDev = sd(Response),
                   Mean = mean(Response),
                   Resp1 = length(Response[Response==1])/length(Response),
                   Resp2 = length(Response[Response==2])/length(Response),
                   Resp3 = length(Response[Response==3])/length(Response),
                   Resp4 = length(Response[Response==4])/length(Response),
                   Resp5 = length(Response[Response==5])/length(Response),
                   Resp6 = length(Response[Response==6])/length(Response),
                   Resp7 = length(Response[Response==7])/length(Response)
  )
  
  # Modularize 
  all.last <- subset(alldata,Year==min(Year))
  all.this <- subset(alldata,Year==max(Year))
  
  
  pval <- welchEMJ(all.this$Mean,all.last$Mean,all.this$StdDev,all.last$StdDev,all.this$N,all.last$N)$p
  MeanStar = ifelse(pval<.05,"\\*","")
  
  phrases <- list("learning objectives",
                   "content",
                   "amount of time",
                   "course materials",
                   "actively engaged",
                   "apply the content",
                   c("assessments","expect"),
                   c("assessments","beyond memorization"),
                     "help was available",
                     "Overall")
  
  QuestionWithNum <- with(all.this,paste0(QuestionNo,". ",emphasis(Question,phrases,"md")))

  N <- all.this$N
  NOld <- all.last$N
  Mean <- paste0(sprintf("%.1f",all.this$Mean,1),MeanStar)
  MeanOld <- sprintf("%.1f",all.last$Mean,1)
  StdDev <- sprintf("%.1f",all.this$StdDev,1)
  StdDevOld <- sprintf("%.1f",all.last$StdDev,1)
  Resp1 <- all.this$Resp1
  Resp2 <- all.this$Resp2
  Resp3 <- all.this$Resp3
  Resp4 <- all.this$Resp4
  Resp5 <- all.this$Resp5
  Resp6 <- all.this$Resp6
  Resp7 <- all.this$Resp7
  
  newdata <- data.frame(QuestionWithNum,N,NOld,StdDev,StdDevOld,Mean,MeanOld,
                        Resp1,Resp2,Resp3,Resp4,
                        Resp5,Resp6,Resp7,stringsAsFactors=FALSE)
  
  return(newdata)
  
}