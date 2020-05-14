#' Serial Connection Summary
#'
#' Displays summarized informations and status of the serial connection.
#' 
#' @param object object of type \code{serialConnection}
#' @param ... not used
#'
#' @return Table of connection properties
#'
#' @export
summary.serialConnection <- function(object, ...)
{
  cat(paste(rep("-",33),collapse = ""),"\n"
      ,"   Serial Connection Summary\n\r"
      ,paste(rep("-",33),collapse = ""),"\n",sep = "")
  
  if(object$name != '')
    cat(format("Connection name:",width = 18),object$name,"\n")
  cat(format("Port:",width = 18,zero.print = "."),object$port,"\n")
  
  cat(format("Status:",width = 18),ifelse(isOpen(object),"open","closed"),"\n")
  
  comProperties <- strsplit(object$mode,",")[[1]]
  comProperties[2] <- switch(comProperties[2]
                             ,n = "none"
                             ,o = "odd"
                             ,e = "even"
                             ,m = "mark"
                             ,s = "space"
                      )
  cat(format("Baud rate:",width = 18),comProperties[1],"\n")
  cat(format("Parrity:",width = 18),comProperties[2],"\n")
  cat(format("Data bits:",width = 18),comProperties[3],"\n")
  cat(format("Stop bit(s):",width = 18),comProperties[4],"\n")
  
  cat(format("Handshake:",width = 18),object$handshake,"\n")

  cat(format("Newline:",width = 18),ifelse(object$newline == 0, "no","yes"),"\n")
  if(object$eof != '')
    cat(format("End of Line chr.:",width = 18),object$eof == 0,"\n")
  cat(format("Translation:",width = 18),object$translation,"\n\n")
  
  cat("Buffer Status:\n")
  cat(format("\tBuffering:",width = 18),object$buffering,"\n")
  cat(format("\tSize:",width = 18),object$buffersize,"\n")
  cat(format("\tUsage (in/out):",width = 18)
      ,ifelse(isOpen(object), paste(nBytesInQueue(object),collapse = "/"),"NA/NA"),"\n")
  

}