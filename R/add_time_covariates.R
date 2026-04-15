#' @title Add some covartiates regarded with time
#'
#' @param x A \code{data.frame} with some time (\code{Date}/\code{POSIX}) column.
#' @param time_col Name of the time column. It must be of class \code{Date} or 
#' \code{POSIX}.
#' @param what What extra columns you must want to add?
#' 
#' @details
#' At the current version, \code{add_time_covariates} allows to include the next 
#' variables:
#' 
#' \itemize{
#'  \item \code{month}: Month as \code{numeric}.
#'  \item \code{month_fact}: Month as \code{factor}.
#'  \item \code{month_circ}: Month as \code{numeric} but in a cyclic scale.
#'  \item \code{season}: Season as \code{numeric}.
#'  \item \code{season_fact}: Season as \code{factor}.
#'  \item \code{season_circ}: Season as \code{numeric} but in a cyclic scale.
#' }
#' 
#' If some of the requested variables are already present in \code{x}, you could
#' indicate a new name by setting a name for that column. For instance, if 
#' \code{month} column name is already present in \code{x}, you must indicate
#' \code{what = c(new_month = "month")}.
#' 
#'
#' @returns It returns the same \code{data.frame} with extra columns.
#' @export
#'
#' @examples
#' exDF <- data.frame(a = 1:10, t = Sys.Date()) 
#' exDF2 <- add_time_covariates(x = exDF, time_col = "t")
#' exDF
#' 
#' add_time_covariates(x = exDF2, time_col = "t", what = c(month2 = "month"))
add_time_covariates <- function(
    x,
    time_col = "time", 
    what = c(
      "month", "month_fact", "month_circ", 
      "season", "season_fact", "season_circ"
    )
){
  
  x <- as.data.frame(x)
  
  if(is.null(names(what))){
    names(what) <- what
  }else{
    names(what) <- mapply(
      x = names(what),
      y = what,
      FUN = \(x, y) ifelse(test = x == "", y, x),
      SIMPLIFY = TRUE
    )
  }
  
  if(length(intersect(x = colnames(x), y = names(what))) > 0){
    stop("Some of the required columns: See Details to check how to resolve it.")
  }
  
  timeVct <- x[,time_col]
  
  if(!is.Date(timeVct) && !is.POSIXt(timeVct)){
    stop("`time_col` must refer to a Date or POSIXt column.")
  }
  
  # cat(names(what), sep = "\n")
  
  if(any(grepl(x = what, pattern = "^month"))){
    
    month <- month(timeVct)
    
    if("month" %in% what){
      colname <- names(what)[what %in% "month"]
      x[,colname] <- month
    }
    
    if("month_fact" %in% what){
      colname <- names(what)[what %in% "month_fact"]
      x[,colname] <- factor(x = month, levels = 1:12, ordered = TRUE)
    }
    
    if("month_circ" %in% what){
      colname <- names(what)[what %in% "month_circ"]
      x[,colname] <- cos(month / 12 * 2 * pi)
    }
  }
  
  if(any(grepl(x = what, pattern = "^season"))){
    
    season <- (month(timeVct) - 1) %/% 3 + 1
    
    if("season" %in% what){
      colname <- names(what)[what %in% "season"]
      x[,colname] <- season
    }
    
    if("season_fact" %in% what){
      colname <- names(what)[what %in% "season_fact"]
      x[,colname] <- factor(x = season, levels = 1:4, ordered = TRUE)
    }
    
    if("season_circ" %in% what){
      colname <- names(what)[what %in% "season_circ"]
      x[,colname] <- cos(season / 4 * 2 * pi)
    }
  }
  
  x
}
