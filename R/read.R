#' Reads from the serial interface.
#' 
#' This function reads from the serial interface as long as the buffer is not
#' empty. The read takes place per line in normal operation mode. Here end-of-line
#' characters (\\n;\\r) are clipped according to the settings (\code{translation}).
#' 
#' In binary (hex-) mode the read takes place per byte. The result is a raw 
#' vector of hexadecimal numbers. To get numeric values \code{as.numeric} 
#' function must be invoked.
#' Mind: Values form 0x01 -- 0x31 might be displayed as escaped characters like
#'  "\\001" if they are interpreted as string.
#' If the end-of-file character specified by \code{eof} is received the reading 
#' stops. A \code{close(con)} -- \code{open(con)} sequence must be invoked to 
#' reopen the connection.
#' If \code{n>0} <n> bytes will be read. In case of less than \code{n} bytes 
#' available the function returns the buffer without waiting for all \code{n} 
#' characters.
#' If the result is empty (zero length) then the empty string '' is returned in ASCII
#' mode or \code{NA} in binary mode.
#' 
#' 
#' @param con serial connection
#' @param n number of bytes to read. Only in binary mode. 
#'          \code{n=0} (default) reads the whole buffer at once. 
#' 
#' @usage read.serialConnection(con,n = 0)
#' 
#' @return In normal mode the result is a string. In binary mode raw values will 
#' be returned
#' @seealso \code{\link{serial}}
#' @examples
#'  # See the top package documentation
#' @export
read.serialConnection <- function(con, n = 0)
{
  if(!isOpen(con))
    stop(simpleError(paste(con$port,'is not open!')))
  
  stopifnot(is.numeric(n), n>=0)
  
  res <- NULL
  while(1)
  {
    # read operation enables binary mode
    if(con$translation == 'binary')
    {
      # cu* ... converts bytewise to uint
      # read ... reads the whole buffer, ignoring the translation
      
      comStr <- paste('binary scan [read ${sdev_',con$var,'}'
                      ,ifelse(n > 0,paste(' ',n,sep = ''),'')
                      ,'] cu* tcl_tmp_'
                      ,con$var
                      , sep ='')
      tryCatch( tcl_msg <- tclvalue( .Tcl( comStr ) )
                ,error = function(e) stop(simpleError(e$message))
      )
      if(tcl_msg == "1")
      {
        # read out tcl_tmp and convert to numerical vector
        tmp <- as.numeric(strsplit( tclvalue(paste('tcl_tmp_'
                                                   ,con$var
                                                   ,sep = ''
        )
        )
        ,split = " ")[[1]]
        )
        if(length(tmp) == 0)
          break
        tmp <- as.raw(tmp)
      }
      else
        stop("Error while reading from connection!")
      
      res <- c(res,tmp)
      
      if(n != 0) break
    }
    else
    { # gets operation enables ascii mode with translation
      comStr <- paste('gets ${sdev_',con$var,'}', sep='')
      tryCatch( tmp <- tclvalue( .Tcl( comStr ) )
                ,error = function(e) stop(simpleError(e$message))
      )
      if(tmp == '') break
      res <- paste(res, tmp, sep = '')
    }
  }
  
  if(length(res) == 0)
    res <- ifelse(con$translation == 'binary',NA,'')
  
  return(res)
}
