`[.tabular` <- function(x, i, j, ..., drop=FALSE) {
	
    if (drop) return(unclass(x)[i,j, drop=TRUE])

    attrs <- attributes(x)
    rowLabels <- rowLabels(x)
    colLabels <- colLabels(x)
    x <- unclass(x)[i,j, drop=FALSE]
    attrs$justification <- attrs$justification[i,j, drop=FALSE]
    attrs$dropcells <- attrs$dropcells[i, j, drop=FALSE]
    attrs$formats <- attrs$formats[i,j, drop=FALSE]
    attrs$dim <- dim(x)
    attrs$dimnames <- dimnames(x)

    rowLabels <- rowLabels[i,, drop = FALSE]
    colLabels <- colLabels[,j, drop = FALSE]
    
    # Put it all back together
    attrs$rowLabels <- unclass(rowLabels)
    attrs$colLabels <- unclass(colLabels)
    attributes(x) <- attrs
    x
}

cbind.tabular <- function(..., deparse.level = 1) {
    args <- list(...)
    if (!length(args)) return(NULL)
    result <- NULL
    while (length(args)) {
	x <- args[[1]]
	args <- args[-1]
	if (is.null(x)) next
	if (is.null(result)) {
	    attrs <- attributes(x)
	    result <- unclass(x)
	    fmtlist <- attr(attrs$table, "fmtlist")
	} else {	
	    xattrs <- attributes(x)	
	    if (nrow(result) != nrow(x) || !identical(attrs$rowLabels, xattrs$rowLabels) )
		stop("Cannot cbind if tables have different rows")
	    result <- cbind(result, unclass(x))

	    attrs$justification <- cbind(attrs$justification, xattrs$justification)
	    attrs$dropcells <- cbind(attrs$dropcells, xattrs$dropcells)   
	    attrs$formats <- cbind(attrs$formats, xattrs$formats + length(fmtlist))
	    attrs$dim <- dim(result)
	    attrs$dimnames <- dimnames(result)
	
	    fmtlist <- c(fmtlist, attr(xattrs$table, "fmtlist"))
	    attrs$table <- c(attrs$table, xattrs$table)
	
	    # rowLabels are fine
	
	    colLabels <- attrs$colLabels
	    cattrs <- attributes(colLabels)
	    xcolLabels <- xattrs$colLabels
	    xcattrs <- attributes(xcolLabels)
	    colLabels <- cbind(colLabels, xcolLabels)
	    cattrs$dim <- dim(colLabels)
	    cattrs$dimnames <- dimnames(colLabels)
	    cattrs$justification <- cbind(cattrs$justification, xcattrs$justification)

	    attributes(colLabels) <- cattrs	
	    attrs$colLabels <- colLabels
	    attr(attrs$table, "fmtlist") <- fmtlist
	}
    }
    if (!is.null(result)) 
	attributes(result) <- attrs
    result
}

rbind.tabular <- function(..., deparse.level = 1) {
    args <- list(...)
    if (!length(args)) return(NULL)
    result <- NULL
    while (length(args)) {
	x <- args[[1]]
	args <- args[-1]	
	if (is.null(x)) next
	if (is.null(result)) {
	    attrs <- attributes(x)
	    result <- unclass(x)
	    fmtlist <- attr(attrs$table, "fmtlist")    
	} else {
	    xattrs <- attributes(x)	
	    if (ncol(result) != ncol(x) || !identical(attrs$colLabels, xattrs$colLabels) )
		stop("Cannot rbind if tables have different columns")
	    result <- rbind(result, unclass(x))

	    attrs$justification <- rbind(attrs$justification, xattrs$justification)
	    attrs$dropcells <- rbind(attrs$dropcells, xattrs$dropcells)
	    attrs$formats <- rbind(attrs$formats, xattrs$formats + length(fmtlist))
	    attrs$dim <- dim(result)
	    attrs$dimnames <- dimnames(result)
	
	    fmtlist <- c(fmtlist, attr(xattrs$table, "fmtlist"))
	    attrs$table <- c(attrs$table, xattrs$table)
	
	    rowLabels <- attrs$rowLabels
	    rattrs <- attributes(rowLabels)
	    xrowLabels <- xattrs$rowLabels
	    xrattrs <- attributes(xrowLabels)
	    rowLabels <- rbind(rowLabels, xrowLabels)
	    rattrs$dim <- dim(rowLabels)
	    rattrs$dimnames <- dimnames(rowLabels)
	    rattrs$justification <- rbind(rattrs$justification, xrattrs$justification)

	    # colLabels are fine
	
	    attributes(rowLabels) <- rattrs	
	    attrs$rowLabels <- rowLabels
	    attr(attrs$table, "fmtlist") <- fmtlist
	}
    }	
    if (!is.null(result)) 
	attributes(result) <- attrs
    result
}

rowLabels <- function(x) {
  structure(attr(x, "rowLabels"), class = c("tabularRowLabels", "tabularLabels"))
}

`rowLabels<-` <- function(x, value) {
  stopifnot(inherits(value, "tabularRowLabels"),
  	    dim(x)[1] == dim(value)[1])
  attr(x, "rowLabels") <- unclass(value)
  x
}

colLabels <- function(x) {
	structure(attr(x, "colLabels"), class = c("tabularColLabels", "tabularLabels"))
}

`colLabels<-` <- function(x, value) {
	stopifnot(inherits(value, "tabularColLabels"),
		  dim(x)[2] == dim(value)[2])
	attr(x, "colLabels") <- unclass(value)
	x
}

`[.tabularColLabels` <- function(x, i, j, ..., drop=FALSE) {
  if (drop) return(unclass(x)[i,j, ..., drop=TRUE])
  
  colLabels <- unclass(x) 
  a <- attributes(x)
  
  # This is the tricky bit
  origrows <- row(colLabels)
  dimnames(origrows) <- dimnames(colLabels)
  origcols <- col(colLabels)
  for (k in seq_len(ncol(origcols))[-1]) 
    origcols[, k] <- ifelse(is.na(colLabels[, k]), origcols[, k-1], origcols[, k])
  origrows <- origrows[i,j, ..., drop=FALSE]
  origcols <- origcols[i,j, ..., drop=FALSE]
  colLabels <- colLabels[cbind(c(origrows), c(origcols))]
  dim(colLabels) <- dim(origrows)
  for (k in seq_len(ncol(origcols))[-1])
    colLabels[, k] <- ifelse(origcols[, k] == origcols[, k-1], NA, colLabels[, k])
  dimnames(colLabels) <- list(dimnames(origrows)[[1]], NULL)
  
  justification <- a$justification
  a$justification <- justification[i, j, ..., drop = FALSE]
  a[c("dim", "dimnames")] <- NULL
  attributes(colLabels) <- c(attributes(colLabels), a)
  colLabels
}

`[.tabularRowLabels` <- function(x, i, j, ..., drop=FALSE) {
  if (drop) return(unclass(x)[i,j, ..., drop=TRUE])

  rowLabels <- unclass(x) 
  a <- attributes(x)
  
  # This is the tricky bit
  origrows <- row(rowLabels)
  dimnames(origrows) <- dimnames(rowLabels)
  origcols <- col(rowLabels)
  for (k in seq_len(nrow(origrows))[-1]) 
    origrows[k,] <- ifelse(is.na(rowLabels[k,]), origrows[k-1,], origrows[k,])
  origrows <- origrows[i,j, ..., drop=FALSE]
  origcols <- origcols[i,j, ..., drop=FALSE]
  rowLabels <- rowLabels[cbind(c(origrows), c(origcols))]
  dim(rowLabels) <- dim(origrows)
  for (k in seq_len(nrow(origrows))[-1])
    rowLabels[k,] <- ifelse(origrows[k,] == origrows[k-1,], NA, rowLabels[k,])
  dimnames(rowLabels) <- list(NULL, dimnames(origrows)[[2]])

  justification <- a$justification
  a$justification <- justification[i, j, ..., drop = FALSE]
  a[c("dim", "dimnames")] <- NULL
  attributes(rowLabels) <- c(attributes(rowLabels), a)
  rowLabels
}


print.tabularLabels <- function(x, ...) {
  attrnames <- names(attributes(x))
  delnames <- setdiff(attrnames, c("dim", "dimnames"))
  attributes(x)[delnames] <- NULL
  x <- noquote(x)
  class(x) <- setdiff(class(x), "tabularRowLabels")
  print(x, ...)
  cat(paste("Attributes: ", 
  	  paste(attrnames, collapse = ", "), "\n"))
}

