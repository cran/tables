### R code from vignette source 'tables.Rnw'

###################################################
### code chunk number 1: tables.Rnw:21-22
###################################################
options(width=60)


###################################################
### code chunk number 2: tables.Rnw:42-43
###################################################
library(tables)


###################################################
### code chunk number 3: iris
###################################################
tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
         (Sepal.Length + Sepal.Width)*(mean + sd), data=iris )


###################################################
### code chunk number 4: tables.Rnw:53-56
###################################################
latex(
tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
         (Sepal.Length + Sepal.Width)*(mean + sd), data=iris )
)


###################################################
### code chunk number 5: tables.Rnw:63-64 (eval = FALSE)
###################################################
## booktabs()


###################################################
### code chunk number 6: tables.Rnw:68-70
###################################################
saved.options <- table_options()
booktabs()


###################################################
### code chunk number 7: irisbook
###################################################
latex(
tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
         (Sepal.Length + Sepal.Width)*(mean + sd), data=iris )
)


###################################################
### code chunk number 8: tables.Rnw:134-141
###################################################
set.seed(100)
X <- rnorm(10)
X
A <- sample(letters[1:2], 10, rep=TRUE)
A
F <- factor(A)
F


###################################################
### code chunk number 9: tables.Rnw:265-266
###################################################
saved.options


###################################################
### code chunk number 10: tables.Rnw:271-272
###################################################
table_options()[c("toprule", "midrule", "bottomrule", "titlerule")]


###################################################
### code chunk number 11: tables.Rnw:276-277 (eval = FALSE)
###################################################
## latex(
## tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
##          (Sepal.Length + Sepal.Width)*(mean + sd), data=iris )
## )


###################################################
### code chunk number 12: tables.Rnw:280-281
###################################################
latex(
tabular( (Species + 1) ~ (n=1) + Format(digits=2)*
         (Sepal.Length + Sepal.Width)*(mean + sd), data=iris )
)


###################################################
### code chunk number 13: split (eval = FALSE)
###################################################
## latex(tabular(Species ~ (n=1) + Format(digits=2)*
##          (Sepal.Length + Sepal.Width)*(mean + sd), data=iris),
##       options=list(doFooter=FALSE, doEnd=FALSE))
## cat("\\ \\\\ \\multicolumn{6}{l}{
## \\textit{Overall, we see the following: }} \\\\
## \\ \\\\")
## latex(tabular(1 ~ (n=1) + Format(digits=2)*
##          (Sepal.Length + Sepal.Width)*(mean + sd), data=iris),
##       options=list(doBegin=FALSE, doHeader=FALSE))


###################################################
### code chunk number 14: tables.Rnw:299-300
###################################################
latex(tabular(Species ~ (n=1) + Format(digits=2)*
         (Sepal.Length + Sepal.Width)*(mean + sd), data=iris),
      options=list(doFooter=FALSE, doEnd=FALSE))
cat("\\ \\\\ \\multicolumn{6}{l}{
\\textit{Overall, we see the following: }} \\\\
\\ \\\\")
latex(tabular(1 ~ (n=1) + Format(digits=2)*
         (Sepal.Length + Sepal.Width)*(mean + sd), data=iris),
      options=list(doBegin=FALSE, doHeader=FALSE))


###################################################
### code chunk number 15: tables.Rnw:352-353
###################################################
latex( tabular(F + 1 ~ 1) )


###################################################
### code chunk number 16: tables.Rnw:368-369
###################################################
latex( tabular( X*F*(mean + sd) ~ 1 ) )


###################################################
### code chunk number 17: tables.Rnw:381-382
###################################################
latex( tabular( X*F ~ mean + sd ) )


###################################################
### code chunk number 18: tables.Rnw:396-397
###################################################
latex( tabular( X*(Newname=F) ~ mean + sd ) )


###################################################
### code chunk number 19: tables.Rnw:419-420
###################################################
latex( tabular( (F+1) ~ (n=1) + X*(mean + sd) ) )


###################################################
### code chunk number 20: tables.Rnw:432-435
###################################################
latex( tabular( (i = factor(seq_along(X)))  ~ 
       Heading()*identity*(X+A + 
              (F = as.character(F) ) ) ) ) 


###################################################
### code chunk number 21: tables.Rnw:445-447
###################################################
latex( tabular( (X > 0) + (X < 0)  + 1
    ~ ((n = 1) + X*(mean + sd)) ) )


###################################################
### code chunk number 22: tables.Rnw:476-478
###################################################
latex( tabular( I(X > 0) + I(X < 0)  
    ~ ((n=1) + mean + sd) ) )


###################################################
### code chunk number 23: tables.Rnw:511-513
###################################################
latex( tabular( (F+1) ~ (n=1) 
           + Format(digits=2)*X*(mean + sd) ) )


###################################################
### code chunk number 24: tables.Rnw:524-533
###################################################
stderr <- function(x) sd(x)/sqrt(length(x))
fmt <- function(x, digits, ...) {
  s <- format(x, digits=digits, ...)
  is_stderr <- (1:length(s)) > length(s) %/% 2
  s[is_stderr] <- sprintf("$(%s)$", s[is_stderr])
  s[!is_stderr] <- latexNumeric(s[!is_stderr])
  s
}
latex( tabular( Format(fmt(digits=1))*(F+1) ~ X*(mean + stderr) ) )


###################################################
### code chunk number 25: tables.Rnw:547-549
###################################################
latex( tabular( (F+1) ~ X*(Format(digits=2)*mean 
                    + (n=1) + .Format(1)*sd) ) )


###################################################
### code chunk number 26: tables.Rnw:571-573
###################################################
latex( tabular( (Heading("$\\Phi$")*F+1) ~ (n=1) 
           + Format(digits=2)*Heading()*X*(mean + sd) ) )


###################################################
### code chunk number 27: tables.Rnw:587-589
###################################################
latex( tabular( Justify(r)*(F+1) ~ Justify(c)*(n=1) 
   + Justify(c,r)*Format(digits=2)*X*(mean + sd) ) )


###################################################
### code chunk number 28: tables.Rnw:607-612
###################################################
latex( tabular( (Factor(gear, "Gears") + 1)
          *((n=1) + Percent() 
            + (RowPct=Percent("row")) + (ColPct=Percent("col"))) 
         ~ (Factor(carb, "Carburetors") + 1)
          *Format(digits=1), data=mtcars ) )


###################################################
### code chunk number 29: tables.Rnw:655-656
###################################################
latex( tabular( Species ~ Heading()*mean*All(iris), data=iris) )


###################################################
### code chunk number 30: tables.Rnw:681-683
###################################################
latex( tabular( Species + Hline(2:5) + 1 
                         ~ Heading()*mean*All(iris), data=iris) )


###################################################
### code chunk number 31: tables.Rnw:722-725
###################################################
stderr <- function(x) sd(x)/sqrt(length(x))
latex( tabular( (Species+1) ~ All(iris)*
          PlusMinus(mean, stderr, digits=1), data=iris ) )


###################################################
### code chunk number 32: tables.Rnw:757-764
###################################################
lcl <- function(x) mean(x) - qt(0.975, df=length(x)-1)*stderr(x)
ucl <- function(x) mean(x) + qt(0.975, df=length(x)-1)*stderr(x)
latex( tabular( (Species+1) ~ All(iris)*
          Paste(lcl, ucl, digits=2, 
                head="95\\% CI", sep=",", prefix="[",
                postfix="]"), 
          data=iris ) )


###################################################
### code chunk number 33: tables.Rnw:808-811
###################################################
subset <- 1:15
latex( tabular( RowFactor(subset, "$i$", spacing=5)  ~ 
       All(iris[subset,], factor=as.character)*Heading()*identity ) )


###################################################
### code chunk number 34: tables.Rnw:818-826
###################################################
dat <- expand.grid(Block=1:3, Treatment=LETTERS[1:2], 
                                Subset=letters[1:2])
dat$Response <- rnorm(12)
latex( tabular( RowFactor(Block, spacing=1)
                * RowFactor(Treatment, spacing=1, space=0.5)
                * Factor(Subset)
                ~ Response*Heading()*identity, data=dat),
                options=list(rowlabeljustification="c"))


###################################################
### code chunk number 35: tables.Rnw:848-856
###################################################
subset <- 1:50
latex( tabular( RowFactor(subset, "$i$", spacing=5, 
                                             suppressfirst=FALSE)  ~ 
       All(iris[subset,], factor=as.character)*Heading()*identity ),
       options = list(tabular="longtable",
          toprule="\\caption{This table crosses page boundaries.}\\\\
              \\toprule",
midrule="\\midrule\\\\[-2\\normalbaselineskip]\\endhead\\hline\\endfoot") )


###################################################
### code chunk number 36: tables.Rnw:865-869
###################################################
subset <- 1:10
latex( tabular( Factor(subset)  ~ 
       All(iris[subset,], factor=as.character)*Heading()*identity, 
       suppress=3 ) )


###################################################
### code chunk number 37: tables.Rnw:881-884
###################################################
latex( tabular( Multicolumn(Species, width=3, 
            levelnames=paste("\\textit{Iris", levels(Species),"}")) 
            * (mean + sd)  ~ All(iris), data=iris, suppress=1))


###################################################
### code chunk number 38: tables.Rnw:931-932
###################################################
df <- data.frame(A = factor(c( "$", "\\" ) ), B_label=1:2)


###################################################
### code chunk number 39: tables.Rnw:935-936 (eval = FALSE)
###################################################
## latex( tabular( mean ~ A*B_label, data=df ) ) 


###################################################
### code chunk number 40: tables.Rnw:940-941
###################################################
latex( tabular( mean ~ Factor(A)*All(df), data=df ) ) 


###################################################
### code chunk number 41: tables.Rnw:959-962
###################################################
dat <- data.frame( a = c(1, 2, 3, NA), b = 1:4 )
mean(dat$a)
mean(dat$a, na.rm=TRUE)


###################################################
### code chunk number 42: tables.Rnw:970-972
###################################################
Mean <- function(x) base::mean(x, na.rm=TRUE)
latex( tabular( Mean ~ a + b, data=dat ) )


###################################################
### code chunk number 43: tables.Rnw:978-979
###################################################
latex( tabular( mean ~ a + b, data = na.omit(dat) ) )


###################################################
### code chunk number 44: tables.Rnw:985-988
###################################################
latex( tabular( 
  Mean ~ (1 + Heading(Complete)*complete.cases(dat)) * (a + b), 
               data=dat ) )


###################################################
### code chunk number 45: tables.Rnw:998-1006
###################################################
q <- data.frame(p = rep(c("A","B"),each=10,len=30),
                           a = rep(c(1,2,3),each=10),id=seq(30),
                           b = round(runif(30,10,20)),
                           c = round(runif(30,40,70)),
		stringsAsFactors = FALSE)
tab <- tabular((Factor(p)*Factor(a)+1) 
                        ~ (N = 1) + (b + c)*(mean+sd),data=q)
latex(tab)


###################################################
### code chunk number 46: tables.Rnw:1012-1013
###################################################
latex(tab[ tab[,1] > 0, ])


###################################################
### code chunk number 47: tables.Rnw:1020-1025
###################################################
formula <- Factor(p)*Factor(a) ~ 
	   (N = 1) + (b + c)*(mean+sd)
tab1 <- tabular( formula, data = subset(q, p == "A") )
tab2 <- tabular( formula, data = subset(q, p == "B") )
latex(rbind(tab1, tab2))


