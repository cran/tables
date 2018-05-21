knit_print.tabular <- function(x, format = getKnitrFormat(), ...) {
  opts <- table_options()
  if (opts$knit_print) {
    if (format == "latex") {
      lines <- paste(capture.output(latex(x)), collapse = "\n")
      lines <- paste0(lines, "\n\n")
      return(knitr::asis_output(lines))
    } else if (format == "html") {
      save <- table_options(doHTMLheader = FALSE,
                            doHTMLbody = FALSE)
      id <- if (opts$doCSS) basename(tempfile("table"))
      lines <- capture.output(html(x, id = id))  
      table_options(save)
      lines <- paste0(paste(lines, collapse = "\n"), "\n\n")
      return(knitr::asis_output(lines))
    }
  }
  knitr::normal_print(x)
}
