v = .7
lambda <- 0.5
T_0 <- 100

# #https://stats.stackexchange.com/questions/148997/poisson-process-in-r-from-exponential-distribution
# ## find the number 'n' of exponential r.vs required by imposing that
# ## Pr{N(t) <= n} <= 1 - eps for a small 'eps'

# TASK 2

n <- qpois(1 - 1e-8, lambda = v * T_0) # nie wiem czemu jest 1 - 1e-8
X <- rexp(n = n, rate = v)
inspection_times <- c(0, cumsum(X))

lightbulb = 1
failureTimes = c(0)
lightbulbTime <- rexp(1, rate = lambda)
lightbulbLifetimes <- c()
for(i in 1:(length(inspection_times) - 1)){
  if( inspection_times[i + 1] - failureTimes[lightbulb] > lightbulbTime){
    failureTimes = append(failureTimes, inspection_times[i + 1])
    lightbulb = lightbulb + 1
    lightbulbLifetimes = append(lightbulbLifetimes, lightbulbTime)
    lightbulbTime <- rexp(1, rate = lambda)
  }
}

plot(inspection_times)
points(failureTimes, col = 'red')

#TASK 3

momentsOfFailure <- lightbulbLifetimes + failureTimes[1:length(failureTimes)-1]
percentageOfTimeWithoutLight <- sum(failureTimes[2:length(failureTimes)] - momentsOfFailure) / tail(failureTimes, n=1)

N = 100
avgNoOfReplacements = 0
for(j in 1:N){
    n <- qpois(1 - 1e-8, lambda = v * T_0) # nie wiem czemu jest 1 - 1e-8
    X <- rexp(n = n, rate = v)
    inspection_times <- c(0, cumsum(X))

    lightbulb = 1
    failureTimes = c(0)
    lightbulbTime <- rexp(1, rate = lambda)
    lightbulbLifetimes <- c()
    for(i in 1:(length(inspection_times) - 1)){
        if( inspection_times[i + 1] - failureTimes[lightbulb] > lightbulbTime){
            failureTimes = append(failureTimes, inspection_times[i + 1])
            lightbulb = lightbulb + 1
            lightbulbLifetimes = append(lightbulbLifetimes, lightbulbTime)
            lightbulbTime <- rexp(1, rate = lambda)
        }
    }
    avgNoOfReplacements = avgNoOfReplacements + lightbulb
}

avgNoOfReplacements = avgNoOfReplacements / N