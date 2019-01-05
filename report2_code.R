alpha = .05

t.power1 = function(nsamp=c(200,200),nsim=1000,means=c(0,0),sds=c(1,1),var.equal=TRUE){
  tps = replicate(nsim,
                  t.test(rnorm(nsamp[1],mean=means[1],sd=sds[1]),
                         rnorm(nsamp[2],mean=means[2],sd=sds[2]), var.equal = var.equal)$p.value)
  
  sum(tps < alpha/2 | tps > 1 - alpha/2) / nsim
}

wilcoxon.power = function(nsamp=c(200,200),nsim=1000,means=c(0,0),sds=c(1,1)){
  tps = replicate(nsim,
                  wilcox.test(rnorm(nsamp[1],mean=means[1],sd=sds[1]),
                              rnorm(nsamp[2],mean=means[2],sd=sds[2]))$p.value)
  
  sum(tps < alpha/2 | tps > 1 - alpha/2) / nsim
}

## TASK 1
powers_student <- vector(mode="numeric", length=0)
powers_welch <- vector(mode="numeric", length=0)
powers_wilcoxon <- vector(mode="numeric", length=0)
for (i in seq(-2,2,.1)){
  powers_student = c(powers_student, t.power1(means = c(0, i), sds = c(2,2)))
  powers_welch = c(powers_welch, t.power1(means = c(0, i), sds = c(2,2), var.equal = FALSE))
  powers_wilcoxon = c(powers_wilcoxon, wilcoxon.power(means = c(0, i), sds = c(2,2)))
}

plot(seq(-2,2,.1), powers_student)
plot(seq(-2,2,.1), powers_welch)
plot(seq(-2,2,.1), powers_wilcoxon)

##TASK 2
powers_student <- vector(mode="numeric", length=0)
powers_welch <- vector(mode="numeric", length=0)
powers_wilcoxon <- vector(mode="numeric", length=0)
for (i in seq(-2,2,.1)){
  powers_student = c(powers_student, t.power1(means = c(0, i), sds = c(2,4)))
  powers_welch = c(powers_welch, t.power1(means = c(0, i), sds = c(2,4), var.equal = FALSE))
  powers_wilcoxon = c(powers_wilcoxon, wilcoxon.power(means = c(0, i), sds = c(2,4)))
}

plot(seq(-2,2,.1), powers_student)
plot(seq(-2,2,.1), powers_welch)
plot(seq(-2,2,.1), powers_wilcoxon)

##TASK 3
powers_student <- vector(mode="numeric", length=0)
powers_welch <- vector(mode="numeric", length=0)
powers_wilcoxon <- vector(mode="numeric", length=0)

for (i in seq(0,4,.1)){
  tps = replicate(1000,
                  t.test(rexp(200,rate = 1/2),
                         rexp(200,rate = 1/i), var.equal = TRUE)$p.value)
  powers_student = c(powers_student, sum(tps < alpha/2 | tps > 1 - alpha/2) / 1000)
}

for (i in seq(0,4,.1)){
  tps = replicate(1000,
                  t.test(rexp(200,rate = 1/2),
                         rexp(200,rate = 1/i), var.equal = FALSE)$p.value)
  powers_welch = c(powers_welch, sum(tps < alpha/2 | tps > 1 - alpha/2) / 1000)
}

for (i in seq(0,4,.1)){
  tps = replicate(1000,
                  wilcox.test(rexp(200,rate = 1/2),
                              rexp(200,rate = 1/i))$p.value)
  powers_wilcoxon = c(powers_wilcoxon, sum(tps < alpha/2 | tps > 1 - alpha/2) / 1000)
}

plot(seq(0,4,.1), powers_student)
plot(seq(0,4,.1), powers_welch)
plot(seq(0,4,.1), powers_wilcoxon)

#power.t.test
