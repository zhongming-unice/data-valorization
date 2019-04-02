# ex1.1
# data <- c(23,24,24,25,24,24,24,25,26,22,21)
# #Center/location
# mean(data)
# median(data)
# sd(data)
# var(data)
# IQR(data)
# #Rank
# min(data)
# max(data)
# quantile(data,0.25)

# ex1.2
# n = 50
# k = 1000
# mu = 5; sigma = 2; sdclt = sigma/sqrt(n)
# x = matrix(rnorm(n*k,mu,sigma),n,k)
# x.mean = apply(x,2,mean)
# x.down = mu - 4*sdclt; x.up = mu + 4*sdclt; y.up = 1.5
# hist(x.mean,prob = T,xlim = c(x.down,x.up),ylim = c(0,y.up),main = '1111')
# 
# par(new = T)
# x = seq(x.down,x.up,0.01)
# y = dnorm(x,mu,sdclt)
# plot(x,y,type ='l',xlim = c(x.down,x.up),ylim = c(0,y.up))

# ex1.3
# data(iris)
# dim(iris)
# class(iris)
# summary(iris)
# head(iris)
# tail(iris)
# sapply(iris, class)
# plot(iris$Sepal.Length)
# plot(iris$Sepal.Length,iris$Sepal.Width)
# library(RColorBrewer)
# display.brewer.all(n=3)
# plot(Sepal.Width ~ Sepal.Length, data = iris, col = brewer.pal(3,"Set2")[iris$Species])
# legend(x = 6.5 ,y = 4.5, legend = levels(iris$Species),col = brewer.pal(3,"Set2"),pch =1)
# x<- hist(iris$Sepal.Length)
# segments(x0 = x$mids-0.25,x1 = x$mids+0.25,y0 = x$counts,y1 = x$counts,lw = 4, col = 'red')

# ex1.4
setwd("C:/Users/kamibukuro233/data_v/tp")
flights <- read.csv('2008.csv.bz2')
str(flights)
summary(flights)
dim(flights)
nrow(flights)
ncol(flights)
names(flights)
col1 <- flights[,1]
is.na(col1)
c(NA,1:nrow(flights)) %in% col1
length(which(is.na(flights)))
which(is.na(flights))

for (i in 1:ncol(flights)){
  c <- flights[,i]
  if (length(which(is.na(c))) != 0){
    print(names(flights)[i])
  }
}
