v = .7
lambda <- 0.5
T_0 <- 100

# #https://stats.stackexchange.com/questions/148997/poisson-process-in-r-from-exponential-distribution
# ## find the number 'n' of exponential r.vs required by imposing that
# ## Pr{N(t) <= n} <= 1 - eps for a small 'eps'
n <- qpois(1 - 1e-8, lambda = lambda * T_0) # nie wiem czemu jest 1 - 1e-8
# 
# ## simulate exponential interarrivals the
# X <- rexp(n = n, rate = lambda)
# lightbulbsTimes <- c(0, cumsum(X))
# plot(x = lightbulbsTimes, y = 0:n, type = "s", xlim = c(0, T_0)) 
# 
n <- qpois(1 - 1e-8, lambda = v * T_0) 

## simulate exponential interarrivals the
X <- rexp(n = n, rate = v)
inspection_times <- c(0, cumsum(X))
#plot(x = lightbulbsTimes, y = 0:n, type = "s", xlim = c(0, T_0))


#inspection_times[2:(end(inspection_times)[1])] - inspection_times[1:(end(inspection_times)[1] - 1)]
n <- qpois(1 - 1e-8, lambda = lambda * T_0)# nie wiem czemu jest 1 - 1e-8
lightbulbsTimes <- rexp(n = n, rate = lambda)

lightbulb = 1
failureTimes = c(0)
for(i in 1:(length(inspection_times) - 1)){
  if( inspection_times[i + 1] - failureTimes[lightbulb] > (lightbulbsTimes[lightbulb + 1] - failureTimes[lightbulb])){
    failureTimes = append(failureTimes, inspection_times[i + 1])
    lightbulb = lightbulb + 1
  }
}