data(EuStockMarkets)
mode(EuStockMarkets)
class(EuStockMarkets)
plot(EuStockMarkets)


logR = diff(log(EuStockMarkets))  ###y2-y1, y3-y2, y4-y1,..........cumsum, exponential,,, cumsum  = y1, y1+y2, y3+y2+y1,
plot(logR)##y1, y2-y1, y3-y2,
##y1, y2-y1+y1 = y2, y3-y2+y2-y1+y1 = y3

logR1 = log(EuStockMarkets)
plot(logR1)

y = 5
al

plot(as.data.frame(logR))
##data frame can not be used directly to store time series data
##plot(as.data.frame(as.ts(logR)))  check it

par(mfrow=c(2, 2))
for(i in colnames(logR))
{
  qqnorm(logR[ ,i], datax = T, main = i)
  qqline(logR[ ,i], datax = T)
  print(shapiro.test(logR[ ,i]))
}



n=dim(logR)[1]
q_grid = (1:n) / (n + 1)
df_grid = c(1, 4, 6, 10, 20, 30)
index.names = dimnames(logR)[[2]]
for(i in 1:4)
{
par(mfrow = c(3, 2))
 for(df in df_grid)
    {
      qqplot(logR[,i], qt(q_grid,df),    #inside qt, input is quantile and output is quantile value
             main = paste(index.names[i], ", df = ", df) )
      abline(lm(qt(c(0.25, 0.75), df = df) ~ quantile(logR[,i], c(0.25, 0.75))))
    }
}


library("fGarch")
x=seq(-0.1, 0.1,by = 0.001)
par(mfrow = c(1, 1))
df = 5
mad_t = mad(logR[ , 1],
            constant = sqrt(df / (df - 2)) / qt(0.75, df))
plot(density(logR[ , 1]), lwd = 2, ylim = c(0, 60))

lines(x, dstd(x, mean = mean(logR[,1]), sd = mad_t, nu = df),
        lty = 5, lwd = 2, col = "red")
lines(x, dnorm(x, mean = mean(logR[ ,1]), sd = sd(logR[ ,1])),
         lty = 3, lwd = 4, col = "blue")
legend("topleft", c("KDE", paste("t: df = ",df), "normal"),
        lwd = c(2, 2, 4), lty = c(1, 5, 3),
        col = c("black", "red", "blue"))
#Probability density is the relationship between observations and their probability.