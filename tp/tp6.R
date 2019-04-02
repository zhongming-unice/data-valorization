n <- 1000
mu <- 1.2
m0 <- 0.5
m1 <- 1.1
beta0 <- 0.3
beta1 <- 1.7

x0 <- matrix(rnorm(n, mean = m0, sd = mu),nrow = n,ncol = 1)
x1 <- matrix(rnorm(n,mean = m1, sd = mu),nrow = n,ncol = 1)
x <- rbind(x0,x1)

p0 <- matrix(1 / (1 + exp(-beta0-beta1*x0)),nrow = n,ncol = 1)
p1 <- matrix(1 / (1 + exp(-beta0-beta1*x1)),nrow = n,ncol = 1)

y0 <- matrix(rbinom(n,1,p0),nrow = n,ncol = 1)
y1 <- matrix(rbinom(n,1,p1),nrow = n,ncol = 1)
y <- rbind(y0,y1)

n_iter <- 300

beta <- matrix(numeric(2), nrow = 2)
a <- rep(1,2*n)
h <- matrix(c(a,x), nrow = 2*n, ncol = 2)

step <- 0.02

J <- matrix(0,n_iter,1)
grad_J <- matrix(0,n_iter,1)
max_beta <- matrix(0,2,n_iter)

p_hat <- 

for (i in 1:n_iter) {
  p_hat[i] <- 1/(1+exp(-h %*% beta))
  G <- matrix(colSums(repmat(y-p_hat[i],1,2)*h),nrow = 2,ncol = 1)
  Gamma <- sqrt(G[1]**2 + G[2]**2)
  J[i] <- Gamma
  beta <- beta + (step/Gamma)*G
  max_beta[i] <- beta
  J[i] <- sum(y*log(p_hat[i])) + (1-y)*log(1-p_hat[i])
}



