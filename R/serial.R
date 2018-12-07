#' Wirtes data to serial interface.
#' 
#' Writes to a serial connection in ascii or binary mode.
#' 
#' In normal operation mode (non-binary ascii mode) \code{write.serialConnection} 
#' respects the translation and adds the end-of-line characters (\\n;\\r) according 
#' to the settings. Any input is converted to character. As \code{c(1,2,3)} becomes
#' \code{'123'} and so on.
#' 
#' In binary mode \emph{no} end-of-line characters are added. The input argument
#' must be of type \code{raw} or \code{string}. If \code{dat} is numeric 
#' (vector) it is converted to \code{raw}.
#' 
#' @param con serial Connection
#' @param dat data string to write on the serial interface. This must be a string
#'            \code{'...'} in case of ascii communication. In case of binary 
#'            communication also numeric vectors are allowed. See examle section
#'            in \code{\link{serial}}.
#' 
#' @usage write.serialConnection(con,dat)
#' @return The status of success 'DONE' or 'Nothing to do' is returned.
#' @seealso \code{\link{serial}}
#' @examples
#'  # See the top package documentation
#'  
#'  \dontrun{write.serialConnection(con, 'Hello World!')}
#' @export
write.serialConnection<-function(con,dat)
{
  if(!isOpen(con))
    stop(simpleError(paste(con$port,'is not open!')))
  
  if(is.null(dat))
    return(message('Nothing to do!'))
  
  if(dat == '' & con$newline == 0)
    return(message('Nothing to do!'))

  # Tcl / Tk requires a character representation of the data
  if(con$translation == 'binary' & !is.character(dat))
  {
    dat <- as.raw(dat)
  }
  
  if(is.raw(dat))
  {
    # convert to escaped acii strings
    dat <- paste('\\x',paste(dat,collapse = "\\x"),sep="")
  }
  
  dat <- paste(dat,collapse = '',sep = '') # collapse all to one string

  nl <- '-nonewline '
  if(con$newline) nl <- ''
  
  tryCatch({
            # convert to byte array
            .Tcl( paste('binary scan \"',dat,'\" a* tcl_tmp_',con$port,sep='') )
            .Tcl( paste('puts ',nl,'${sdev_',con$port, '} $tcl_tmp_'
                        ,con$port
                        ,sep='')
                  )
           }
            ,error = function(e) stop(simpleError(e$message))
  )
  #   ..,' \"', dat,'\"',.. -> quotes dat in TCL String
  #   with out quoting space and control characters this will fail 
  
  invisible('DONE')
}

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
#' stops. A \code{close(con)} -- \code{open(con)} combination must be done to 
#' reopen the connection.
#' If \code{n>0} <n> bytes will be read. In case of less than \code{n} bytes 
#' available the function returns the buffer without waiting for all \code{n} 
#' characters.
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
      # binary scan [read $sdev_CNCA0] cu* data -- converts bytewise to uint
      
      comStr <- paste('binary scan [read ${sdev_',con$port,'}'
                                   ,ifelse(n > 0,paste(' ',n,sep = ''),'')
                                   ,'] cu* tcl_tmp_'
                      ,con$port
                      , sep ='')
      tryCatch( tcl_msg <- tclvalue( .Tcl( comStr ) )
                ,error = function(e) stop(simpleError(e$message))
                )
      if(tcl_msg == "1")
      {
        # read out tcl_tmp and convert to numerical vector
        tmp <- as.numeric(strsplit( tclvalue(paste('tcl_tmp_'
                                                   ,con$port
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
      comStr <- paste('gets ${sdev_',con$port,'}', sep='')
      tryCatch( tmp <- tclvalue( .Tcl( comStr ) )
                ,error = function(e) stop(simpleError(e$message))
      )
      if(tmp == '') break
      res <- paste(res, tmp, sep = '')
    }
  }
  
  return(res)
}

#' Flushes the connection.
#' 
#' Some times (and depending on buffering mode) the connection buffer needs 
#' to be flushed manually. This command empties the buffer by sending all 
#' remaining bytes.
#' 
#' @param con serial connection
#' 
#' @method flush serialConnection
#' 
#' @return Nothing is returned
#' @seealso \code{\link{serial}}
#' @examples
#'  # See the top package documentation
#' @export
flush.serialConnection <- function(con)
{
  if(!isOpen(con))
    stop(simpleError(paste(con$port,'is not open!')))
  
  tryCatch({
    .Tcl( paste( 'flush ${sdev_', con$port,'}', sep = '' ) )
  } 
  ,error = function(e) stop(simpleError(e$message))
  )
  
  invisible('DONE')
}


#' Reads out the number of characters / bytes pending in the input and output buffer
#' 
#' @param con serial connection
#' 
#' @return named vector with number of bytes
#' @seealso \code{\link{serial}}
#' @examples
#'  # See the top package documentation
#' @export
nBytesInQueue <- function(con)
{
  if(!isOpen(con))
    stop(simpleError(paste(con$port,'is not open!')))
  
  tryCatch({
    tmp <- tclvalue(.Tcl( paste( 'fconfigure ${sdev_', con$port,'} -queue', sep = '' ) ) )
  } 
  ,error = function(e) stop(simpleError(e$message))
  )
  
  res <- as.numeric(strsplit(tmp," ")[[1]])
  names(res) <- c("n_in","n_out")
  return(res)
}