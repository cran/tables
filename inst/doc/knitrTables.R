## ----setup, include=FALSE---------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(width=60)

## ----echo = FALSE-----------------------------------------
if (!requireNamespace("rmarkdown") || !rmarkdown::pandoc_available("1.12.3")) {
  warning("This vignette requires pandoc version 1.12.3; code will not run in older versions.")
  knitr::opts_chunk$set(eval = FALSE)
}

## ----echo=FALSE-------------------------------------------
library(tables)
table_options(knit_print = FALSE)

## ----iris-------------------------------------------------
tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
         (Sepal.Length + Sepal.Width)*(mean + sd), data=iris )

## ----echo=FALSE, results='asis'---------------------------
toLatex(
  tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
           (Sepal.Length + Sepal.Width)*(mean + sd), data=iris )
  )

## ---------------------------------------------------------
table_options(knit_print = TRUE)

## ----eval=FALSE-------------------------------------------
# booktabs()

## ----irisbook,echo=FALSE----------------------------------
saved.options <- table_options()
invisible(booktabs())
tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
         (Sepal.Length + Sepal.Width)*(mean + sd), data=iris )

## ----results = 'markup'-----------------------------------
set.seed(100)
X <- rnorm(10)
X
A <- sample(letters[1:2], 10, replace = TRUE)
A
F <- factor(A)
F

## ----echo=FALSE-------------------------------------------
saved.options

## ----echo=FALSE-------------------------------------------
table_options()[c("toprule", "midrule", "bottomrule", "titlerule")]

## ----irisbook, eval=FALSE---------------------------------
# saved.options <- table_options()
# invisible(booktabs())
# tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
#          (Sepal.Length + Sepal.Width)*(mean + sd), data=iris )

## ----split, eval=FALSE------------------------------------
# toLatex(tabular(Species ~ (n=1) + Format(digits=2)*
#          (Sepal.Length + Sepal.Width)*(mean + sd), data=iris),
#       options=list(doFooter=FALSE, doEnd=FALSE))
# cat("\\ \\\\ \\multicolumn{6}{l}{
# \\textit{Overall, we see the following: }} \\\\
# \\ \\\\")
# toLatex(tabular(1 ~ (n=1) + Format(digits=2)*
#          (Sepal.Length + Sepal.Width)*(mean + sd), data=iris),
#       options=list(doBegin=FALSE, doHeader=FALSE))

## ----echo=FALSE, results = "asis"-------------------------
toLatex(tabular(Species ~ (n=1) + Format(digits=2)*
         (Sepal.Length + Sepal.Width)*(mean + sd), data=iris),
      options=list(doFooter=FALSE, doEnd=FALSE))
cat("\\ \\\\ \\multicolumn{6}{l}{
\\textit{Overall, we see the following: }} \\\\
\\ \\\\")
toLatex(tabular(1 ~ (n=1) + Format(digits=2)*
         (Sepal.Length + Sepal.Width)*(mean + sd), data=iris),
      options=list(doBegin=FALSE, doHeader=FALSE))

## ----eval=FALSE-------------------------------------------
# latexNumeric(chars, minus = TRUE, leftpad = TRUE, rightpad=TRUE,
#                         mathmode = TRUE)

## ---------------------------------------------------------
tabular(F + 1 ~ 1)

## ---------------------------------------------------------
tabular( X*F*(mean + sd) ~ 1 )

## ---------------------------------------------------------
tabular( X*F ~ mean + sd )

## ---------------------------------------------------------
tabular( X*(Newname=F) ~ mean + sd )

## ---------------------------------------------------------
tabular( (F+1) ~ (n=1) + X*(mean + sd) )

## ---------------------------------------------------------
tabular( (i = factor(seq_along(X)))  ~ 
       Heading()*identity*(X+A + 
              (F = as.character(F) ) ) )

## ---------------------------------------------------------
tabular( (X > 0) + (X < 0)  + 1
    ~ ((n = 1) + X*(mean + sd)) )

## ---------------------------------------------------------
tabular( I(X > 0) + I(X < 0)
    ~ ((n=1) + mean + sd) )

## ---------------------------------------------------------
tabular( (F+1) ~ (n=1) 
           + Format(digits=2)*X*(mean + sd) )

## ---------------------------------------------------------
StdErr <- function(x) sd(x)/sqrt(length(x))
fmt <- function(x, digits, ...) {
  s <- format(x, digits=digits, ...)
  is_stderr <- (1:length(s)) > length(s) %/% 2
  s[is_stderr] <- sprintf("$(%s)$", s[is_stderr])
  s[!is_stderr] <- latexNumeric(s[!is_stderr])
  s
}
tabular( Format(fmt(digits=1))*(F+1) ~ X*(mean + StdErr) )

## ---------------------------------------------------------
tabular( (F+1) ~ X*(Format(digits=2)*mean 
                    + (n=1) + .Format(1)*sd) )

## ---------------------------------------------------------
tabular( (Heading("$\\Phi$")*F+1) ~ (n=1) 
           + Format(digits=2)*Heading()*X*(mean + sd) )

## ---------------------------------------------------------
tabular( X*F + Heading("near")*X 
		+ Heading("far", nearData = FALSE)*X ~ mean + sd )

## ---------------------------------------------------------
tabular( Justify(r)*(F+1) ~ Justify(c)*(n=1) 
   + Justify(c,r)*Format(digits=2)*X*(mean + sd) )

## ----eval=FALSE-------------------------------------------
# tabular( (Factor(gear, "Gears") + 1)
#           *((n=1) + Percent()
#             + (RowPct=Percent("row"))
#             + (ColPct=Percent("col")))
#          ~ (Factor(carb, "Carburetors") + 1)
#           *Format(digits=1), data=mtcars )

## ---------------------------------------------------------
tabular( (Factor(gear, "Gears") + 1)
          *((n=1) + Percent() 
            + (RowPct=Percent(Equal(gear)))  # Equal, not "row"
            + (ColPct=Percent(Equal(carb)))) # Equal, not "col"
         ~ (Factor(carb, "Carburetors") + 1)
          *Format(digits=1), data=mtcars )

## ---------------------------------------------------------
# This is the example from the weighted.mean help page
wt <- c(5,  5,  4,  1)/15
x <- c(3.7,3.3,3.5,2.8)
gp <- c(1,1,2,2)
tabular( (Factor(gp) + 1) 
                ~ weighted.mean*x*Arguments(w = wt) )

## ----eval=FALSE-------------------------------------------
# tabular( (Factor(gp) + 1)
#                 ~ Arguments(x, w = wt)*weighted.mean )

## ---------------------------------------------------------
set.seed(730)
df <- data.frame(Label = LETTERS[1:9], 
		 Group = rep(letters[1:3], each=3), 
		 Value = rnorm(9), 
		 stringsAsFactors = TRUE)
tabular( Label ~ Group*Value*mean, 
		data = df[1:6,])

## ---------------------------------------------------------
tabular( Label ~ Group*Value*mean*
			DropEmpty(empty="."), 
		data = df[1:6,])

## ---------------------------------------------------------
tabular( Species ~ Heading()*mean*All(iris), data=iris)

## ---------------------------------------------------------
df <- mtcars[1:10,]
tabular(Factor(cyl)*Factor(gear)*AllObs(df) ~ 
               rownames(df) + mpg, data=df)

## ---------------------------------------------------------
rownum <- with(mtcars, RowNum(list(cyl, gear)))
tabular(Factor(cyl)*Factor(gear)*I(rownum) ~
        mpg * AllObs(mtcars, within = list(cyl, gear, rownum)), 
        data=mtcars)

## ---------------------------------------------------------
rownum <- with(mtcars, RowNum(list(cyl, gear), perrow = 2))
tabular(Factor(cyl)*Factor(gear)*
	       AllObs(mtcars, within = list(cyl, gear, rownum)) ~
               mpg * I(rownum), 
        data=mtcars)

## ---------------------------------------------------------
tabular( Species + Hline(2:5) + 1 
                         ~ Heading()*mean*All(iris), data=iris)

## ---------------------------------------------------------
StdErr <- function(x) sd(x)/sqrt(length(x))
tabular( (Species+1) ~ All(iris)*
          PlusMinus(mean, StdErr, digits=1), data=iris )

## ---------------------------------------------------------
lcl <- function(x) mean(x) - qt(0.975, df=length(x)-1)*StdErr(x)
ucl <- function(x) mean(x) + qt(0.975, df=length(x)-1)*StdErr(x)
tabular( (Species+1) ~ All(iris)*
          Paste(lcl, ucl, digits=2, 
                head="95\\% CI", sep=",", prefix="[",
                postfix="]"), 
          data=iris )

## ---------------------------------------------------------
subset <- 1:15
tabular( RowFactor(subset, "$i$", spacing=5)  ~ 
       All(iris[subset,], factor=as.character)*Heading()*identity )

## ----results = "asis"-------------------------------------
set.seed(1000)
dat <- expand.grid(Block=1:3, Treatment=LETTERS[1:2], 
                                Subset=letters[1:2])
dat$Response <- rnorm(12)
toLatex( tabular( RowFactor(Block, spacing=1)
                * RowFactor(Treatment, spacing=1, space=0.5)
                * Factor(Subset)
                ~ Response*Heading()*identity, data=dat),
                options=list(rowlabeljustification="c") )

## ----results = "asis"-------------------------------------
subset <- 1:50
toLatex( tabular( RowFactor(subset, "$i$", spacing=5, 
                                             suppressfirst=FALSE)  ~ 
       All(iris[subset,], factor=as.character)*Heading()*identity ),
       options = list(tabular="longtable",
          toprule="\\caption{This table crosses page boundaries.}\\\\
              \\toprule",
midrule="\\midrule\\\\[-2\\normalbaselineskip]\\endhead\\hline\\endfoot") )

## ---------------------------------------------------------
subset <- 1:10
tabular( Factor(subset)  ~ 
       All(iris[subset,], factor=as.character)*Heading()*identity, 
       suppress=3 )

## ----eval=FALSE-------------------------------------------
# code <- capture.output( toLatex( tab ) )
# code <- sub("^(.*)(\\\\nopagebreak )", "\\2\\1", code)
# cat(code, sep = "\n")

## ---------------------------------------------------------
tabular( Multicolumn(Species, width=3, 
            levelnames=paste("\\textit{Iris", levels(Species),"}")) 
            * (mean + sd)  ~ All(iris), data=iris, suppress=1)

## ---------------------------------------------------------
df <- data.frame(A = factor(c( "$", "\\" ) ), B_label=1:2)

## ----eval=FALSE-------------------------------------------
# tabular( mean ~ A*B_label, data=df )

## ---------------------------------------------------------
options(tables.texify = TRUE)
tabular( mean ~ Factor(A)*All(df), data=df )

## ---------------------------------------------------------
dat <- data.frame( a = c(1, 2, 3, NA), b = 1:4 )
mean(dat$a)
mean(dat$a, na.rm=TRUE)

## ---------------------------------------------------------
Mean <- function(x) base::mean(x, na.rm=TRUE)
tabular( Mean ~ a + b, data=dat )

## ---------------------------------------------------------
tabular( mean ~ a + b, data = na.omit(dat) )

## ---------------------------------------------------------
tabular( 
  Mean ~ (1 + Heading(Complete)*complete.cases(dat)) * (a + b), 
               data=dat )

## ---------------------------------------------------------
A <- factor(dat$a)
tabular( A + 1 ~ (n=1))
A <- factor(dat$a, exclude = NULL)
tabular( A + 1 ~ (n=1) )

## ---------------------------------------------------------
set.seed(1206)
q <- data.frame(p = rep(c("A","B"), each = 10, length.out = 30),
                           a = rep(c(1,2,3),each = 10),id = seq(30),
                           b = round(runif(30,10,20)),
                           c = round(runif(30,40,70)),
		stringsAsFactors = FALSE)
tab <- tabular((Factor(p)*Factor(a)+1) 
                ~ (N = 1) + (b + c)*(mean+sd), data = q)
tab

## ---------------------------------------------------------
tab[ tab[,1] > 0, ]

## ---------------------------------------------------------
formula <- Factor(p)*Factor(a) ~ 
	   (N = 1) + (b + c)*(mean+sd)
tab <- NULL
for (sub in c("A", "B")) 
    tab <- rbind(tab, tabular( formula, 
                               data = subset(q, p == sub) ) )
tab

## ---------------------------------------------------------
colLabels(tab)
labs <- colLabels(tab)
labs[1, 2] <- "New label"
colLabels(tab) <- labs

## ---------------------------------------------------------
tab

## ---------------------------------------------------------
library(magrittr)
library(kableExtra)
toKable(tab) %>% 
  kable_styling(full_width = TRUE) %>%
  column_spec(4, color = "red")

## ---------------------------------------------------------
latexTable(tabular((Species + 1) ~ (n=1) + Format(digits=2)*
                   (Sepal.Length + Sepal.Width)*(mean + sd), 
                   data=iris),
           caption = "Iris sepal data", label = "sepals")

