dev.off()
n = rnorm(10000)  ##sample data point
qqnorm(n2,datax=TRUE)  ##drawing normal prob plot, y axis is from n
qqline(n2)

n1=rt(10000,4)  ###generating value from t distribution
n2 = rt(10000, 12)

data(SP500, package = "Ecdat")
SPreturn = SP500$r500
qqnorm(SPreturn,datax = TRUE)
qqline(SPreturn)


n = length(SPreturn)
year_SP = 1981 + (1:n) * (1991.25 - 1981) / n
plot(year_SP, SPreturn, main = "S&P 500 daily returns",
     xlab = "year", type = "l", ylab = "log return")

data(Garch, package = "Ecdat")
attach(Garch)
diffdm = diff(dm) # Deutsch mar


dev.off()
qqplot(SPreturn, diffdm, xlab = "S&P return",
       ylab = "change in DM/dollar rate", main = "(a)")
xx = quantile(SPreturn, c(0.25, 0.75))
yy = quantile(diffdm, c(0.25, 0.75))
slope = (yy[2] - yy[1]) / (xx[2] - xx[1])
inter = yy[1] - slope*xx[1]
abline(inter, slope, lwd = 2 )

