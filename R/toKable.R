getKnitrFormat <- function(default = "latex") {
  options <- opts_knit$get(c("out.format", "rmarkdown.pandoc.to"))
  if (identical(options$out.format, "markdown"))
    return(options$rmarkdown.pandoc.to)
  if (!is.null(options$out.format))
    return(options$out.format)
  default
}

toKable <- function(table, format = getKnitrFormat(), booktabs = TRUE)
{ 
  if (!inherits(table, "tabular"))
    stop("'table' must be a 'tabular' object.")
	
  format <- match.arg(format, c("latex", "html", "markdown", "pandoc",
  			        "rst"))
  if (!(format %in% c("latex", "html")))
    stop("Only 'latex' and 'html' format are currently supported.")
  
  if (format == "latex") {
    save <- if (booktabs) booktabs() else table_options()
    lines <- paste0(capture.output(latex(table)), collapse = "\n")
    table_options(save)
  } else
    lines <- paste0(capture.output(html(table)), collapse = "\n")
  structure(lines,
	  format = format, class = "knitr_kable",
	  n_head = nrow(attr(table, "colLabels")))
}