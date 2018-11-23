
#Q1
pbin <- function(x, n, p){
  a <- c()
  for (i in 0:x){
    pr <- factorial(n) / (factorial(i) * factorial(n - i)) * p^i * (1 - p)^(n-i)
    a[i+1] <- pr
  }
  return(sum(a))
}

pbin(5, 20, 0.2)
pbinom(5, 20, 0.2)




#Q2
power <- function(n, delta, sd, sig.level){
  p <- c()
  for (i in 1 : 10000){
    x <- rnorm(n, 0, sd)
    p[i] <- t.test(x, mu = delta, conf.level = (1 - sig.level))$p.value
  }
  return(length(which(p < 0.05)) / 10000)
}

power(n = 30, delta = 0.5, sd = 1, sig.level = 0.05)
power.t.test(n = 30, delta = 0.5, sd = 1, sig.level = 0.05, type = "one.sample")$power
