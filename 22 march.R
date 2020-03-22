setwd("C:/Users/Gaurish/Desktop/Teaching Meterial/datasets")
data = read.csv("Stock_bond.csv", header = TRUE)
head(data)
attach(data)
par(mfrow=c(1,2))
plot(GM_AC)
plot(F_AC)

n = dim(data)[1]
GMReturn = GM_AC[-1] / GM_AC[-n] - 1  ##net returns
FReturn = F_AC[-1] / F_AC[-n] - 1
par(mfrow = c(1, 1))
plot(GMReturn,FReturn)

##return vs log return of GM
GMReturn_netreturn  = GMReturn + 1
log_GMReturn = log(GMReturn_netreturn)
plot(GMReturn, log_GMReturn)  ##simply return means net return and log return means log of gross return
cor(GMReturn, log_GMReturn)

##Hedge fund Simulation, simulating log normal geomatric random walk process for stock prices
niter = 1e5 # number of iterations
below = rep(0, niter) # set up storage
set.seed(2009)
for (i in 1:niter)
  {
    r = rnorm(45, mean = 0.05/253,
                sd = 0.23/sqrt(253)) # generate number from normal, hence they are iid with N(0.05, 0.23^2)
    logPrice = log(1e6) + cumsum(r) #replicating equation 2.7
    minlogP = min(logPrice) # minimum price over next 45 days
    below[i] = as.numeric(minlogP < log(950000))
    }
mean(below)

#simulating random walk
set.seed(2012)
n = 253
par(mfrow=c(3,3))
for (i in (1:9))
  {
    logr = rnorm(n, 0.05 / 253, 0.2 / sqrt(253))   #r1, R2, R3....
    price = c(120, 120 * exp(cumsum(logr)))
    plot(price, type = "b")
}
