diamonds_data = read.csv("diamonds.csv", header = TRUE)
#diamonds_data = read.csv("D:/Dokumenty/MATEMATYKA/Statistical Packages/PWR_StatisticalPackages-master/diamonds.csv", header = TRUE)
#library(rcompanion)
library('corrplot')
library('coefplot')
library(glmulti)
library(car)

#check outliers
diamonds_data <- diamonds_data[2:11] #data cleaning - removing unnecessary variables
#changing 0 to NAs
diamonds_data$z[diamonds_data$z==0] = NA
diamonds_data$x[diamonds_data$x==0] = NA
diamonds_data$y[diamonds_data$y==0] = NA
any(is.na(diamonds_data)) #check if there are any nulls in our dataset

#checking for typos
summary(diamonds_data$cut)
summary(diamonds_data$color)
summary(diamonds_data$clarity)

#datachecking - histograms
hist(diamonds_data$depth) # guassian
hist(diamonds_data$carat) # exponential
hist(diamonds_data$table) # almost gaussian
hist(diamonds_data$x) # not take into consideration distribution closer to uniform than to others
hist(diamonds_data$y) # 2 outliers hard to say anything about dostribution
hist(diamonds_data$z) # similarly to y
plot(summary(diamonds_data$cut))
plot(summary(diamonds_data$color))
plot(summary(diamonds_data$clarity))

# check correlation between variables
diamonds_data_omited <- na.omit(diamonds_data)
variables_data_frame <- data.frame(diamonds_data_omited$carat, diamonds_data_omited$depth, diamonds_data_omited$table, log(diamonds_data_omited$price), diamonds_data_omited$x, diamonds_data_omited$y, diamonds_data_omited$z)
corelations <- cor(variables_data_frame)
corrplot(corelations, method = 'circle')

standarized_price <- diamonds_data$price / max(diamonds_data$price) #it's obviously not normalization
fitted_model <- glm(diamonds_data$price ~ diamonds_data$carat + diamonds_data$x, family = gaussian())

fitted_model <- glm(diamonds_data$price ~ diamonds_data$depth , family = poisson())
summary(fitted_model)

fitted_model <- glm(diamonds_data$price ~ diamonds_data$x + diamonds_data$y , family = gaussian())
summary(fitted_model)

fitted_model <- glm(diamonds_data$price ~ diamonds_data$x + diamonds_data$y + diamonds_data$z, family = gaussian())
summary(fitted_model)


#step(model.nnull, scope = list(upper = model.full), direction = 'both', test = 'criteria', data)

simplest_model <- glm(formula = diamonds_data_omited$price ~ 1, data = diamonds_data)
complex_model <- glm(formula = diamonds_data_omited$price ~ diamonds_data_omited$carat + diamonds_data_omited$depth + diamonds_data_omited$table + diamonds_data_omited$x + diamonds_data_omited$y + diamonds_data_omited$z, data = diamonds_data)
#complex_model <- glm(formula = diamonds_data_omited$price ~ ., data = diamonds_data)
forwards <- step(simplest_model, scope=list(lower=formula(simplest_model),upper=formula(complex_model)), direction="both")

forwards

simplest_model <- glm(formula = diamonds_data_omited$price ~ 1, family = inverse.gaussian(link = 'identity'))
complex_model <- glm(formula = diamonds_data_omited$price ~ diamonds_data_omited$x + diamonds_data_omited$y + diamonds_data_omited$z, family = gaussian())
complex_model <- glm(formula = diamonds_data_omited$price ~ diamonds_data_omited$carat + diamonds_data_omited$depth + diamonds_data_omited$table, family = inverse.gaussian(link = 'identity'))
forwards = step(simplest_model, scope=list(lower=formula(simplest_model),upper=formula(complex_model)), direction="forward")

simplest_model <- glm(formula = diamonds_data_omited$price ~ 1, data = diamonds_data)
complex_model <- glm(formula = diamonds_data_omited$price ~ factor(diamonds_data_omited$clarity) + factor(diamonds_data_omited$color) + factor(diamonds_data_omited$cut), data = diamonds_data)
#complex_model <- glm(formula = diamonds_data_omited$price ~ ., data = diamonds_data)
forwards <- step(simplest_model, scope=list(lower=formula(simplest_model),upper=formula(complex_model)), direction="both")

extract.coef(forwards)

ordered_diamonds <- diamonds_data_omited[order(diamonds_data_omited$price),]
prediction <- ordered_diamonds$carat * 10987.11 + ordered_diamonds$x * (-1437.36) + ordered_diamonds$depth * (-205.71) + ordered_diamonds$table * (-101.88) + ordered_diamonds$y * (84.52) + 21476.93


our_simple_model <- glm(diamonds_data_omited$price ~ diamonds_data_omited$carat + diamonds_data_omited$depth)

our_prediction <- 4063.3 + ordered_diamonds$carat * 7764.5 + ordered_diamonds$depth * (-102.4)


###############################################################################################
#use of glmmulti (probably it needs java)

result_BIC_gaussian <- glmulti('price', xr = c('x', 'y', 'z', 'carat', 'depth', 'table'), data = diamonds_data_omited, level = 1, crit = 'bic' , maxsize = 2, family = poisson())
result_AIC_gaussian <- glmulti('price', xr = c('x', 'y', 'z', 'carat', 'depth', 'table'), data = diamonds_data_omited, level = 1, crit = 'aic' , maxsize = 2) #family = poisson())

result_BIC_gaussian_with_categorical <- glmulti('price', xr = c('x', 'y', 'z', 'carat', 'depth', 'table', 'cut', 'color', 'clarity'), data = diamonds_data_omited, level = 1, crit = 'bic' , maxsize = 4) #, family = quasipoisson())
result_AIC_gaussian_with_categorical <- glmulti('price', xr = c('x', 'y', 'z', 'carat', 'depth', 'table', 'cut', 'color', 'clarity'), data = diamonds_data_omited, level = 1, crit = 'aic' , maxsize = 4) #family = poisson())

#rest of data discarded. The criteria was that that data didn't occur in top models
result_BIC_gaussian_with_interactions <- glmulti('price', xr = c('x', 'z', 'carat', 'cut', 'color', 'clarity'), data = diamonds_data_omited, level = 2, crit = 'bic' , maxsize = 4, method = 'g') #family = poisson())
result_AIC_gaussian_with_interactions <- glmulti('price', xr = c('x', 'z', 'carat', 'cut', 'color', 'clarity'), data = diamonds_data_omited, level = 2, crit = 'aic' , maxsize = 3) #family = poisson())

weightable(result_BIC_gaussian)
weightable(result_BIC_gaussian_with_categorical)
weightable(result_BIC_gaussian_with_interactions)



#############################################################
#test of different families/link functions
result_BIC <- glmulti('price', xr = c('x', 'y', 'z', 'carat', 'depth', 'table'), data = diamonds_data_omited, level = 1, crit = 'bic' , maxsize = 3, family = poisson(link = 'identity'))
weightable(result_BIC) #best 16945653

result_BIC <- glmulti('price', xr = c('x', 'y', 'z', 'carat', 'depth', 'table'), data = diamonds_data_omited, level = 1, crit = 'bic' , maxsize = 3, family = poisson(link = 'sqrt'))
weightable(result_BIC) #it needs sth more to provide , so not completed <<<<--------------

# poisson with identity produces NaNs

result_BIC <- glmulti('price', xr = c('x', 'y', 'z', 'carat', 'depth', 'table'), data = diamonds_data_omited, level = 1, crit = 'bic' , maxsize = 3, family = Gamma())
weightable(result_BIC) #it needs sth more to provide (or producing NaNs), so not completed <<<<--------------
#the same with identity

result_BIC <- glmulti('price', xr = c('x', 'y', 'z', 'carat', 'depth', 'table'), data = diamonds_data_omited, level = 1, crit = 'bic' , maxsize = 3, family = Gamma(link = 'log'))
weightable(result_BIC) #best 848887.7 <<<<<<<--------------- THIS IS THE BEST--------

result_BIC <- glmulti('price', xr = c('x', 'y', 'z', 'carat', 'depth', 'table'), data = diamonds_data_omited, level = 1, crit = 'bic' , maxsize = 3, family = inverse.gaussian())
weightable(result_BIC) #it needs sth more to provide (or producing NaNs), so not completed <<<<--------------
#the same with identity
#log produces different error, but works for single glm fit

result_BIC <- glmulti('price', xr = c('x', 'y', 'z', 'carat', 'depth', 'table'), data = diamonds_data_omited, level = 1, crit = 'bic' , maxsize = 3, family = gaussian())
weightable(result_BIC) #best 942378.4

glm(formula = diamonds_data_omited$price ~ diamonds_data_omited$x + diamonds_data_omited$z + diamonds_data_omited$carat, family = Gamma(link = 'log'))
#strange thing is that calling glm with variables from the best fit produces NaNs
result_BIC@objects[1]

test <- glm(formula = price~color+clarity+x+carat, data = diamonds_data_omited, family = gaussian())
summary(test)
price ~ 1 + carat + color:carat + clarity:carat
test2 <- glm(formula = price~z+x+carat, data = diamonds_data_omited, family = Gamma(link = 'log'))


#####################################################################################
#model assumptions


model_simple <- glm(formula = price~z+x+carat, data = diamonds_data_omited, family = Gamma(link = 'log'))
model_middle <- glm(formula = price~color+clarity+x+carat, data = diamonds_data_omited, family = gaussian())
model_complex <- glm(formula = price~carat + carat:x + carat:color + carat:clarity, data = diamonds_data_omited, family = gaussian())
layout(matrix(1:6, byrow = T, ncol = 3))
plot(model_simple, which = 1:6)

layout(matrix(1:6, byrow = T, ncol = 3))
plot(model_middle, which = 1:6)

layout(matrix(1:6, byrow = T, ncol = 3))
plot(model_complex, which = 1:6)

########################################################################
#outliers handling
outliers_from_simple <- outlierTest(model_simple, n.max = 20) #it computes Bonferroni Outlier Test and prints max values
outliers_indexes_from_simple <- strtoi(labels(outliers_from_simple$p)) #indexes of outliers - we can do sth with them for ex. delete them

outliers_from_middle <- outlierTest(model_middle, n.max = 30)
outliers_indexes_from_middle <- strtoi(labels(outliers_from_middle$p))

outliers_from_complex <- outlierTest(model_complex, n.max = 30)
outliers_indexes_from_complex <- strtoi(labels(outliers_from_complex$p))

#niezależne residua od wartości
#mają być asymptotycznie normalne
#if family == gaussian than on scale-location plot no patterns
#copulat test
#gtest, fishers_indepedence test or sth like that