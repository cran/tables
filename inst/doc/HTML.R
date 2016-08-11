## ----results='hide', message=FALSE---------------------------------------
library(tables)

## ------------------------------------------------------------------------
X <- rnorm(125, sd=100)
Group <- factor(sample(letters[1:5], 125, rep=TRUE))
tab <- tabular( Group ~ (N=1)+Format(digits=2)*X*((Mean=mean) + Heading("Std Dev")*sd) )

## ------------------------------------------------------------------------
table_options(htmloptions(head=FALSE))

## ----results='asis'------------------------------------------------------
writeCSS()
html(tab)

## ----results='asis'------------------------------------------------------
table_options(htmloptions(head = FALSE, justification = "r"))
html(tab)

## ----results='asis'------------------------------------------------------
table_options(htmloptions(head = FALSE, justification = "c", pad = TRUE))
html(tab)

## ----comment=NA, echo=FALSE----------------------------------------------
cat(table_options()$CSS)

