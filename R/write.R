#' Wirtes data to serial interface.
#' 
#' Writes to a serial connection in ascii or binary mode.
#' 
#' In normal operation mode (non-binary ascii mode) \code{write.serialConnection} 
#' respects the translation and adds the end-of-line characters (\\n;\\r) according 
#' to the settings. Any input is converted to character, i.e. \code{c(1,2,3)} becomes
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
write.serialConnection <- function(con,dat)
{
  if(!isOpen(con))
    stop(simpleError(paste(con$port,'is not open!')))
  
  if(is.null(dat))
    return(message('Nothing to do!'))
  
  if(all(dat == '') & con$newline == 0)
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
            .Tcl( paste('binary scan \"',dat,'\" a* tcl_tmp_',con$var,sep='') )
            .Tcl( paste('puts ',nl,'${sdev_',con$var, '} $tcl_tmp_'
                        ,con$var
                        ,sep='')
                  )
           }
            ,error = function(e) stop(simpleError(e$message))
  )
  #   ..,' \"', dat,'\"',.. -> quotes dat in TCL String
  #   without quoting space and control characters this will fail 
  
  invisible('DONE')
}

