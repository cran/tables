### R code from vignette source 'tables.Rnw'

###################################################
### code chunk number 1: tables.Rnw:26-27
###################################################
options(width=60)


###################################################
### code chunk number 2: tables.Rnw:50-52
###################################################
library(tables)
suppressWarnings(RNGversion("3.5.3"))


###################################################
### code chunk number 3: iris
###################################################
tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
         (Sepal.Length + Sepal.Width)*(mean + sd), data=iris )


###################################################
### code chunk number 4: tables.Rnw:62-65
###################################################
toLatex(
tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
         (Sepal.Length + Sepal.Width)*(mean + sd), data=iris )
  )


###################################################
### code chunk number 5: tables.Rnw:72-73 (eval = FALSE)
###################################################
## booktabs()


###################################################
### code chunk number 6: tables.Rnw:77-79
###################################################
saved.options <- table_options()
booktabs()


###################################################
### code chunk number 7: irisbook
###################################################
toLatex(
tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
         (Sepal.Length + Sepal.Width)*(mean + sd), data=iris )
)


###################################################
### code chunk number 8: tables.Rnw:147-154
###################################################
set.seed(100)
X <- rnorm(10)
X
A <- sample(letters[1:2], 10, replace = TRUE)
A
F <- factor(A)
F


###################################################
### code chunk number 9: tables.Rnw:294-295
###################################################
saved.options


###################################################
### code chunk number 10: tables.Rnw:302-303
###################################################
table_options()[c("toprule", "midrule", "bottomrule", "titlerule")]


###################################################
### code chunk number 11: tables.Rnw:307-308 (eval = FALSE)
###################################################
## toLatex(
## tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
##          (Sepal.Length + Sepal.Width)*(mean + sd), data=iris )
## )


###################################################
### code chunk number 12: tables.Rnw:311-312
###################################################
toLatex(
tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
         (Sepal.Length + Sepal.Width)*(mean + sd), data=iris )
)


###################################################
### code chunk number 13: split (eval = FALSE)
###################################################
## toLatex(tabular(Species ~ (n=1) + Format(digits=2)*
##          (Sepal.Length + Sepal.Width)*(mean + sd), data=iris),
##       options=list(doFooter=FALSE, doEnd=FALSE))
## cat("\\ \\\\ \\multicolumn{6}{l}{
## \\textit{Overall, we see the following: }} \\\\
## \\ \\\\")
## toLatex(tabular(1 ~ (n=1) + Format(digits=2)*
##          (Sepal.Length + Sepal.Width)*(mean + sd), data=iris),
##       options=list(doBegin=FALSE, doHeader=FALSE))


###################################################
### code chunk number 14: tables.Rnw:330-331
###################################################
toLatex(tabular(Species ~ (n=1) + Format(digits=2)*
         (Sepal.Length + Sepal.Width)*(mean + sd), data=iris),
      options=list(doFooter=FALSE, doEnd=FALSE))
cat("\\ \\\\ \\multicolumn{6}{l}{
\\textit{Overall, we see the following: }} \\\\
\\ \\\\")
toLatex(tabular(1 ~ (n=1) + Format(digits=2)*
         (Sepal.Length + Sepal.Width)*(mean + sd), data=iris),
      options=list(doBegin=FALSE, doHeader=FALSE))


###################################################
### code chunk number 15: tables.Rnw:384-385
###################################################
toLatex( tabular(F + 1 ~ 1) )


###################################################
### code chunk number 16: tables.Rnw:400-401
###################################################
toLatex( tabular( X*F*(mean + sd) ~ 1 ) )


###################################################
### code chunk number 17: tables.Rnw:413-414
###################################################
toLatex( tabular( X*F ~ mean + sd ) )


###################################################
### code chunk number 18: tables.Rnw:428-429
###################################################
toLatex( tabular( X*(Newname=F) ~ mean + sd ) )


###################################################
### code chunk number 19: tables.Rnw:452-453
###################################################
toLatex( tabular( (F+1) ~ (n=1) + X*(mean + sd) ) )


###################################################
### code chunk number 20: tables.Rnw:465-468
###################################################
toLatex( tabular( (i = factor(seq_along(X)))  ~ 
       Heading()*identity*(X+A + 
              (F = as.character(F) ) ) ) ) 


###################################################
### code chunk number 21: tables.Rnw:478-480
###################################################
toLatex( tabular( (X > 0) + (X < 0)  + 1
    ~ ((n = 1) + X*(mean + sd)) ) )


###################################################
### code chunk number 22: tables.Rnw:510-512
###################################################
toLatex( tabular( I(X > 0) + I(X < 0)  
    ~ ((n=1) + mean + sd) ) )


###################################################
### code chunk number 23: tables.Rnw:547-549
###################################################
toLatex( tabular( (F+1) ~ (n=1) 
           + Format(digits=2)*X*(mean + sd) ) )


###################################################
### code chunk number 24: tables.Rnw:560-569
###################################################
stderr <- function(x) sd(x)/sqrt(length(x))
fmt <- function(x, digits, ...) {
  s <- format(x, digits=digits, ...)
  is_stderr <- (1:length(s)) > length(s) %/% 2
  s[is_stderr] <- sprintf("$(%s)$", s[is_stderr])
  s[!is_stderr] <- latexNumeric(s[!is_stderr])
  s
}
toLatex( tabular( Format(fmt(digits=1))*(F+1) ~ X*(mean + stderr) ) )


###################################################
### code chunk number 25: tables.Rnw:583-585
###################################################
toLatex( tabular( (F+1) ~ X*(Format(digits=2)*mean 
                    + (n=1) + .Format(1)*sd) ) )


###################################################
### code chunk number 26: tables.Rnw:617-619
###################################################
toLatex( tabular( (Heading("$\\Phi$")*F+1) ~ (n=1) 
           + Format(digits=2)*Heading()*X*(mean + sd) ) )


###################################################
### code chunk number 27: tables.Rnw:626-628
###################################################
toLatex( tabular( X*F + Heading("near")*X 
		+ Heading("far", nearData = FALSE)*X ~ mean + sd ) )


###################################################
### code chunk number 28: tables.Rnw:642-644
###################################################
toLatex( tabular( Justify(r)*(F+1) ~ Justify(c)*(n=1) 
   + Justify(c,r)*Format(digits=2)*X*(mean + sd) ) )


###################################################
### code chunk number 29: tables.Rnw:684-690 (eval = FALSE)
###################################################
## toLatex( tabular( (Factor(gear, "Gears") + 1)
##           *((n=1) + Percent() 
##             + (RowPct=Percent("row")) 
##             + (ColPct=Percent("col"))) 
##          ~ (Factor(carb, "Carburetors") + 1)
##           *Format(digits=1), data=mtcars ) )


###################################################
### code chunk number 30: tables.Rnw:691-697
###################################################
toLatex( tabular( (Factor(gear, "Gears") + 1)
          *((n=1) + Percent() 
            + (RowPct=Percent(Equal(gear)))  # Equal, not "row"
            + (ColPct=Percent(Equal(carb)))) # Equal, not "col"
         ~ (Factor(carb, "Carburetors") + 1)
          *Format(digits=1), data=mtcars ) )


###################################################
### code chunk number 31: tables.Rnw:721-727
###################################################
# This is the example from the weighted.mean help page
wt <- c(5,  5,  4,  1)/15
x <- c(3.7,3.3,3.5,2.8)
gp <- c(1,1,2,2)
toLatex( tabular( (Factor(gp) + 1) 
                ~ weighted.mean*x*Arguments(w = wt) ) )


###################################################
### code chunk number 32: tables.Rnw:731-733 (eval = FALSE)
###################################################
## toLatex( tabular( (Factor(gp) + 1) 
##                 ~ Arguments(x, w = wt)*weighted.mean ) )


###################################################
### code chunk number 33: tables.Rnw:758-765
###################################################
set.seed(730)
df <- data.frame(Label = LETTERS[1:9], 
		 Group = rep(letters[1:3], each=3), 
		 Value = rnorm(9), 
		 stringsAsFactors = TRUE)
toLatex( tabular( Label ~ Group*Value*mean, 
		data = df[1:6,]))


###################################################
### code chunk number 34: tables.Rnw:770-773
###################################################
toLatex( tabular( Label ~ Group*Value*mean*
			DropEmpty(empty="."), 
		data = df[1:6,]))


###################################################
### code chunk number 35: tables.Rnw:816-817
###################################################
toLatex( tabular( Species ~ Heading()*mean*All(iris), data=iris) )


###################################################
### code chunk number 36: tables.Rnw:830-833
###################################################
df <- mtcars[1:10,]
toLatex( tabular(Factor(cyl)*Factor(gear)*AllObs(df) ~ 
               rownames(df) + mpg, data=df) )


###################################################
### code chunk number 37: tables.Rnw:848-852
###################################################
rownum <- with(mtcars, RowNum(list(cyl, gear)))
toLatex( tabular(Factor(cyl)*Factor(gear)*I(rownum) ~
        mpg * AllObs(mtcars, within = list(cyl, gear, rownum)), 
        data=mtcars) )


###################################################
### code chunk number 38: tables.Rnw:861-866
###################################################
rownum <- with(mtcars, RowNum(list(cyl, gear), perrow = 2))
toLatex( tabular(Factor(cyl)*Factor(gear)*
	       AllObs(mtcars, within = list(cyl, gear, rownum)) ~
               mpg * I(rownum), 
        data=mtcars) )


###################################################
### code chunk number 39: tables.Rnw:891-893
###################################################
toLatex( tabular( Species + Hline(2:5) + 1 
                         ~ Heading()*mean*All(iris), data=iris) )


###################################################
### code chunk number 40: tables.Rnw:932-935
###################################################
stderr <- function(x) sd(x)/sqrt(length(x))
toLatex( tabular( (Species+1) ~ All(iris)*
          PlusMinus(mean, stderr, digits=1), data=iris ) )


###################################################
### code chunk number 41: tables.Rnw:967-974
###################################################
lcl <- function(x) mean(x) - qt(0.975, df=length(x)-1)*stderr(x)
ucl <- function(x) mean(x) + qt(0.975, df=length(x)-1)*stderr(x)
toLatex( tabular( (Species+1) ~ All(iris)*
          Paste(lcl, ucl, digits=2, 
                head="95\\% CI", 
                prefix="[", sep=",", postfix="]"), 
          data=iris ) )


###################################################
### code chunk number 42: tables.Rnw:1018-1021
###################################################
subset <- 1:15
toLatex( tabular( RowFactor(subset, "$i$", spacing=5)  ~ 
       All(iris[subset,], factor=as.character)*Heading()*identity ) )


###################################################
### code chunk number 43: tables.Rnw:1028-1037
###################################################
set.seed(1000)
dat <- expand.grid(Block=1:3, Treatment=LETTERS[1:2], 
                                Subset=letters[1:2])
dat$Response <- rnorm(12)
toLatex( tabular( RowFactor(Block, spacing=1)
                * RowFactor(Treatment, spacing=1, space=0.5)
                * Factor(Subset)
                ~ Response*Heading()*identity, data=dat),
                options=list(rowlabeljustification="c"))


###################################################
### code chunk number 44: tables.Rnw:1059-1067
###################################################
subset <- 1:50
toLatex( tabular( RowFactor(subset, "$i$", spacing=5, 
                                             suppressfirst=FALSE)  ~ 
       All(iris[subset,], factor=as.character)*Heading()*identity ),
       options = list(tabular="longtable",
          toprule="\\caption{This table crosses page boundaries.}\\\\
              \\toprule",
midrule="\\midrule\\\\[-2\\normalbaselineskip]\\endhead\\hline\\endfoot") )


###################################################
### code chunk number 45: tables.Rnw:1076-1080
###################################################
subset <- 1:10
toLatex( tabular( Factor(subset)  ~ 
       All(iris[subset,], factor=as.character)*Heading()*identity, 
       suppress=3 ) )


###################################################
### code chunk number 46: tables.Rnw:1095-1098 (eval = FALSE)
###################################################
## code <- capture.output( toLatex( tab ) )
## code <- sub("^(.*)(\\\\nopagebreak )", "\\2\\1", code)
## cat(code, sep = "\n")


###################################################
### code chunk number 47: tables.Rnw:1104-1107
###################################################
toLatex( tabular( Multicolumn(Species, width=3, 
            levelnames=paste("\\textit{Iris", levels(Species),"}")) 
            * (mean + sd)  ~ All(iris), data=iris, suppress=1))


###################################################
### code chunk number 48: tables.Rnw:1154-1157
###################################################
df <- data.frame(A = factor(c( "$", "\\" ), 
                            levels = c( "$", "\\" ) ),
                 B_label = 1:2)


###################################################
### code chunk number 49: tables.Rnw:1160-1161 (eval = FALSE)
###################################################
## toLatex( tabular( mean ~ A*B_label, data=df ) ) 


###################################################
### code chunk number 50: tables.Rnw:1166-1168
###################################################
options(tables.texify = TRUE)
toLatex( tabular( mean ~ Factor(A)*All(df), data=df ) ) 


###################################################
### code chunk number 51: tables.Rnw:1188-1191
###################################################
dat <- data.frame( a = c(1, 2, 3, NA), b = 1:4 )
mean(dat$a)
mean(dat$a, na.rm=TRUE)


###################################################
### code chunk number 52: tables.Rnw:1199-1201
###################################################
Mean <- function(x) base::mean(x, na.rm=TRUE)
toLatex( tabular( Mean ~ a + b, data=dat ) )


###################################################
### code chunk number 53: tables.Rnw:1207-1208
###################################################
toLatex( tabular( mean ~ a + b, data = na.omit(dat) ) )


###################################################
### code chunk number 54: tables.Rnw:1214-1217
###################################################
toLatex( tabular( 
  Mean ~ (1 + Heading(Complete)*complete.cases(dat)) * (a + b), 
               data=dat ) )


###################################################
### code chunk number 55: tables.Rnw:1227-1231
###################################################
A <- factor(dat$a)
toLatex( tabular( A + 1 ~ (n=1)) )
A <- factor(dat$a, exclude = NULL)
toLatex( tabular( A + 1 ~ (n=1) ) )


###################################################
### code chunk number 56: tables.Rnw:1241-1250
###################################################
set.seed(1206)
q <- data.frame(p = rep(c("A","B"), each = 10, length.out = 30),
                           a = rep(c(1,2,3),each=10),id=seq(30),
                           b = round(runif(30,10,20)),
                           c = round(runif(30,40,70)),
		stringsAsFactors = FALSE)
tab <- tabular((Factor(p)*Factor(a)+1) 
                        ~ (N = 1) + (b + c)*(mean+sd),data=q)
toLatex(tab)


###################################################
### code chunk number 57: tables.Rnw:1256-1257
###################################################
toLatex(tab[ tab[,1] > 0, ])


###################################################
### code chunk number 58: tables.Rnw:1264-1271
###################################################
formula <- Factor(p)*Factor(a) ~ 
	   (N = 1) + (b + c)*(mean+sd)
tab <- NULL
for (sub in c("A", "B")) 
    tab <- rbind(tab, tabular( formula, 
                               data = subset(q, p == sub) ) )
toLatex(tab)


###################################################
### code chunk number 59: tables.Rnw:1277-1281
###################################################
colLabels(tab)
labs <- colLabels(tab)
labs[1, 2] <- "New label"
colLabels(tab) <- labs


###################################################
### code chunk number 60: tables.Rnw:1284-1285
###################################################
toLatex(tab)


###################################################
### code chunk number 61: tables.Rnw:1314-1319
###################################################
library(magrittr)
library(kableExtra)
toKable(tab) %>% 
  kable_styling(full_width = TRUE) %>%
  column_spec(4, color = "red")


###################################################
### code chunk number 62: tables.Rnw:1338-1342
###################################################
latexTable(tabular((Species + 1) ~ (n=1) + Format(digits=2)*
                   (Sepal.Length + Sepal.Width)*(mean + sd), 
                   data=iris),
           caption = "Iris sepal data", label = "sepals")


