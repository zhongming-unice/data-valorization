
M <- 1000
n <- 4
mu <- 3


x <- matrix(rexp(n*M, rate=1/mu),n,M)

u = NULL
for (i in 1:M){
  u[i] = mean(x[1:4,i]);
  }

y <- density(u)

plot(y)

mseq <- c(4,10,50,500,100)
par(mfrow=c(3,2))
for(i in 1:length(mseq)){
  n <- mseq[i]
  x <- matrix(rexp(n*M, rate=1/mu),n,M)
  u <- apply(x, 2, mean)
  y <- density(u)
  plot(y)
}


u.bias <- mean(u) - mu
bias <- rep(0,length(mseq))
varhat <- rep(0,length(mseq))

for(i in 1:length(mseq)){
  n <- mseq[i]
  x <- matrix(rexp(n*M, rate=1/mu),n,M)
  u <- apply(x, 2, mean)
  u.bias <- mean(u) - mu
  u.var <- var(u)
  bias[i] = u.bias
  varhat[i] = u.var
}
