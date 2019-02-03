# #https://stats.stackexchange.com/questions/148997/poisson-process-in-r-from-exponential-distribution
# ## find the number 'n' of exponential r.vs required by imposing that
# ## Pr{N(t) <= n} <= 1 - eps for a small 'eps'

# TASK 2

v <- .7
lambda <- 0.5
T_0 <- 100
# n <- qpois(1 - 1e-8, lambda = v * T_0) # nie wiem czemu jest 1 - 1e-8
# X <- rexp(n = n, rate = v)
# inspection_times <- c(0, cumsum(X))
# inspections <- c(0,sort(runif(n = rpois(1, lambda=time.horizon*nu), min = 0, max = time.horizon)))
inspection_times <- c(0, sort(runif(n = rpois(1, lambda = T_0 * v), min = 0, max = T_0)))

lightbulb <- 1
new_bulb_moments <- c(0)
lightbulbTime <- rexp(1, rate = lambda)
lightbulbLifetimes <- c()
for (i in 1:(length(inspection_times) - 1)){
  if ( inspection_times[i + 1] - new_bulb_moments[lightbulb] > lightbulbTime){
    new_bulb_moments <- append(new_bulb_moments, inspection_times[i + 1])
    lightbulb <- lightbulb + 1
    lightbulbLifetimes <- append(lightbulbLifetimes, lightbulbTime)
    lightbulbTime <- rexp(1, rate = lambda)
  }
}

df_inspection_times <- data.frame(number = 1:length(inspection_times), moments = inspection_times)
ggplot(df_inspection_times, aes(x=moments, y=number)) + geom_point() + labs(x = "Moments of inspection", y = "Number of inspection") + coord_cartesian(xlim = c(3, T_0-3))

momentsOfFailure <- lightbulbLifetimes + new_bulb_moments[1:length(new_bulb_moments) - 1]
df_momentsOfFailure <- data.frame(number = 1:length(momentsOfFailure), moments = momentsOfFailure)
ggplot(df_momentsOfFailure, aes(x=moments, y=number)) + geom_point() + labs(x = "Moments of failure", y = "Number of failure") + coord_cartesian(xlim = c(3, T_0-3))

#TASK 3

momentsOfFailure <- lightbulbLifetimes + new_bulb_moments[1:length(new_bulb_moments) - 1]
percentage_of_time_withoutlight <- sum(new_bulb_moments[2:length(new_bulb_moments)] - momentsOfFailure) / tail(new_bulb_moments, n = 1)

N <- 100
v <- .7
lambda <- 0.5
T_0 <- 100
avg_no_of_replacements <- 0
avg_time_without_light <- 0
for (j in 1:N){
  inspection_times <- c(0, sort(runif(n = rpois(1, lambda = T_0 * v), min = 0, max = T_0)))
  
  lightbulb <- 1
  new_bulb_moments <- c(0)
  lightbulbTime <- rexp(1, rate = lambda)
  lightbulbLifetimes <- c()
  for (i in 1:(length(inspection_times) - 1)){
    if ( inspection_times[i + 1] - new_bulb_moments[lightbulb] > lightbulbTime){
      new_bulb_moments <- append(new_bulb_moments, inspection_times[i + 1])
      lightbulb <- lightbulb + 1
      lightbulbLifetimes <- append(lightbulbLifetimes, lightbulbTime)
      lightbulbTime <- rexp(1, rate = lambda)
    }
  }
  momentsOfFailure <- lightbulbLifetimes + new_bulb_moments[1:length(new_bulb_moments) - 1]
  avg_time_without_light <- avg_time_without_light + mean(new_bulb_moments[2:length(new_bulb_moments)] - momentsOfFailure)
  avg_no_of_replacements <- avg_no_of_replacements + lightbulb
}

avg_no_of_replacements <- avg_no_of_replacements / N
avg_time_without_light <- avg_time_without_light / N


# task 4 - just as I thought - assume that failure is at the moment of inspection


list_of_means <- c()
list_of_vars <- c()
list_of_biases <- c()
list_of_mses <- c()

# for (T_0 in seq(50,250,10)){
for (v in seq(0.1,2.5,0.1)){
# for (lambda in seq(0.1,2.5,0.1)){
  # v <- .7
  lambda <- 0.5
  T_0 <- 100
  # for (j in 1:N){
  inspection_times <- c(0, sort(runif(n = rpois(1, lambda = T_0 * v), min = 0, max = T_0)))
  lightbulb <- 1
  new_bulb_moments <- c(0)
  lightbulbTime <- rexp(1, rate = lambda)
  lightbulbLifetimes <- c()
  for (i in 1:(length(inspection_times) - 1)){
    if ( inspection_times[i + 1] - new_bulb_moments[lightbulb] > lightbulbTime){
      new_bulb_moments <- append(new_bulb_moments, inspection_times[i + 1])
      lightbulb <- lightbulb + 1
      lightbulbLifetimes <- append(lightbulbLifetimes, lightbulbTime)
      lightbulbTime <- rexp(1, rate = lambda)
    }
  }
  
  naive_lightbulb_lifetime <- new_bulb_moments[2:length(new_bulb_moments)] - new_bulb_moments[1:length(new_bulb_moments) - 1]
  list_of_means <- append(list_of_means, mean(naive_lightbulb_lifetime))
  list_of_vars <- append(list_of_vars, var(naive_lightbulb_lifetime))
  
  list_of_biases <- append(list_of_biases, lambda - mean(naive_lightbulb_lifetime))
  list_of_mses <- append(list_of_mses, mean((rep(mean(naive_lightbulb_lifetime),length(naive_lightbulb_lifetime))-naive_lightbulb_lifetime)^2))
}

# df_task4 <- data.frame(time = seq(50,250,10), means = list_of_means, vars = list_of_vars, biases = list_of_biases, mses = list_of_mses)
df_task4 <- data.frame(failure_rate = seq(0.1,2.5,0.1), means = list_of_means, vars = list_of_vars, biases = list_of_biases, mses = list_of_mses)
# df_task4 <- data.frame(inspection_rate = seq(0.1,2.5,0.1), means = list_of_means, vars = list_of_vars, biases = list_of_biases, mses = list_of_mses)
# ggplot(data = df_task4, aes(x = time, y = value)) + 
ggplot(data = df_task4, aes(x = failure_rate, y = value)) + 
# ggplot(data = df_task4, aes(x = inspection_rate, y = value)) +   
  geom_point(aes(y = means, color = "Mean")) +
  geom_point(aes(y = vars, color = "Variance")) +
  geom_point(aes(y = biases, color = "Bias")) +
  geom_point(aes(y = mses, color = "MSE")) +
  # labs(colour="", x = "Time horizon", y = "Value") +
  labs(colour="", x = "Failure rate", y = "Value") +
  # labs(colour="", x = "Inspection rate", y = "Value") +
  theme_bw() + 
  scale_color_manual(values = c("blue","red", "green", "black"))



# task 5 - this estimator assumes that between last inspection and moment of bulb replacement (next inspection) the probability of light failure comes from exponential or uniform distribution

naive_lightbulb_lifetime <- new_bulb_moments[2:length(new_bulb_moments)] - new_bulb_moments[1:length(new_bulb_moments) - 1]
uniform_multiplier <- runif(length(naive_lightbulb_lifetime))
uniform_lightbulb_lifetime <- uniform_multiplier * naive_lightbulb_lifetime
mean_uniform <- mean(uniform_lightbulb_lifetime)
variance_uniform <- var(uniform_lightbulb_lifetime)

#for exponential case basically in the same way, but different multiplier is needed (what multipler??)
# naive_lightbulb_lifetime = new_bulb_moments[2:length(new_bulb_moments)] - new_bulb_moments[1:length(new_bulb_moments) - 1]
# exponential_multiplier = runif(length(naive_lightbulb_lifetime))
# exponential_lightbulb_lifetime = uniform_multiplier * naive_lightbulb_lifetime
# mean_exponential = mean(uniform_lightbulb_lifetime)
# variance_exponential = var(uniform_lightbulb_lifetime)
