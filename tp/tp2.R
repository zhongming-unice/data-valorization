set.seed(1234)
library(ggplot2)

n <- 200
x <- rnorm(n, mean=0.3, sd=1.4)
FF			<-	ecdf(x)			

Ftrue		<-	function(x) {pnorm(x, mean = 0.3, sd = 1.4)}			

alpha		<-	0.05
eps 		<-	sqrt(log(2/alpha)/(2*n))
xx			<-	seq(min(x)-1,max(x)+1,length.out=1000)
ll			<-	pmax(FF(xx)-eps,0) 		
uu 			<-	pmin(FF(xx)+eps,1)

plot(FF, cex=0.25)
lines(xx, ll, col="blue") 	
lines(xx, uu, col="blue")
plot(Ftrue, add=TRUE, col="red", xlim=c(-3,10))


mean(Ftrue(x))
mean(FF(x))
mean(ll)
mean(uu)