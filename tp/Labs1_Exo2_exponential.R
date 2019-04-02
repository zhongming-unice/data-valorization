# Solution of Labs 1, exercise 2, with exponential samples

n <- 300
k <- 10000
lambda <- 2
mu <- 1/lambda
sigma <- 1/lambda
sdclt <- sigma/sqrt(n)

x <- matrix(rexp(n*k,lambda),n,k)

x.mean <- apply(x,2,mean)

x.down <- mu - 4*sdclt
x.up <- mu + 4*sdclt
y.up <- 20
hist(x.mean, breaks = 100, prob = T, xlim=c(x.down,x.up), ylim=c(0,y.up), main="Sampling")

par(new=T)
x <- seq(x.down,x.up,0.01)
y <- dnorm(x,mu,sdclt)
plot(x,y,type='l',xlim=c(x.down,x.up), ylim=c(0,y.up))