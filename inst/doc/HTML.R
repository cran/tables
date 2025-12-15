## ----echo = FALSE-------------------------------------------------------------
if (!requireNamespace("rmarkdown") || !rmarkdown::pandoc_available("1.12.3")) {
  warning("This vignette requires pandoc version 1.12.3; code will not run in older versions.")
  knitr::opts_chunk$set(eval = FALSE)
}

## ----results='hide', message=FALSE--------------------------------------------
library(tables)

## -----------------------------------------------------------------------------
X <- rnorm(125, sd=100)
Group <- factor(sample(letters[1:5], 125, replace = TRUE))
tab <- tabular( Group ~ (N=1)+Format(digits=2)*X*((Mean=mean) + Heading("Std Dev")*sd) )

## -----------------------------------------------------------------------------
table_options(htmloptions(head=FALSE))

## -----------------------------------------------------------------------------
tab

## -----------------------------------------------------------------------------
table_options(knit_print = FALSE)
tab        # This chunk uses the default results = 'markup'

## ----results = 'asis'---------------------------------------------------------
toHTML(tab)  # This chunk uses results = 'asis'

## ----results='asis'-----------------------------------------------------------
writeCSS()

## -----------------------------------------------------------------------------
table_options(htmloptions(head = FALSE, justification = "r", knit_print = TRUE))
tab

## -----------------------------------------------------------------------------
table_options(htmloptions(head = FALSE, justification = "c", pad = TRUE))
tab

## ----comment=NA, echo=FALSE---------------------------------------------------
cat(table_options()$CSS)

## -----------------------------------------------------------------------------
table_options(CSS =
"<style>
#ID .center { 
  text-align:center;
  background-color: aliceblue;
}
</style>", doCSS = TRUE)
tab
table_options(doCSS = FALSE)
tab

## -----------------------------------------------------------------------------
library(magrittr)
library(kableExtra)
toKable(tab, format="html") %>% 
  kable_styling("striped", full_width = FALSE) %>%
  add_header_above(c("Row Label" = 1, "Statistics" = 3)) %>%
  column_spec(4, color = "red") %>%
  row_spec(1, color = "blue") %>%
  group_rows("Subgroup", 3, 5)

## -----------------------------------------------------------------------------
library(magrittr)
library(tinytable)
toTinytable(tab, theme = "striped") %>%
  group_tt(i = list("Subgroup" = 3)) %>%
  group_tt(j = list("Row Label" = 1, "Statistics" = 2:4)) %>%
  style_tt(i = 3, color = "red", align = "c", line = "bt", line_color = "red") %>%
  style_tt(i = 5:6, j = 3:4, background = "black", color = "orange")

