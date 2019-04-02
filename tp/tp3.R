library(ggplot2)
library(MASS)
library("corrgram")
library("corrplot")
library("hexbin")
data("iris")

specie <- table(iris$Species)
pie(specie)
barplot(specie)

par(mfrow=c(2,2))
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Width)


ec1=ecdf(iris$Sepal.Length)
t <- seq(min(iris$Sepal.Length),max(iris$Sepal.Length),length.out = 100)
ec1t=ec1(t)

ec2=ecdf(iris$Petal.Width)
t2 <- seq(min(iris$Petal.Width),max(iris$Petal.Width),length.out = 100)
ec2t=ec2(t2)

par(mfrow=c(1,2))
plot(t,ec1t)
plot(t2,ec2t)

par(mfrow=c(2,2))
plot(seq(0.1,1,0.1),quantile(iris$Sepal.Length,seq(0.1,1,0.1)))
plot(seq(0.1,1,0.1),quantile(iris$Sepal.Width,seq(0.1,1,0.1)))
plot(seq(0.1,1,0.1),quantile(iris$Petal.Length,seq(0.1,1,0.1)))
plot(seq(0.1,1,0.1),quantile(iris$Petal.Width,seq(0.1,1,0.1)))

par(mfrow=c(2,2))
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
boxplot(iris$Petal.Width)

cols <- character(nrow(iris))
cols[] <- "black"
cols[iris$Species == "setosa"] <- "blue"
cols[iris$Species == "virginica"] <- "red"
cols[iris$Species == "versicolor"] <- "green"
pairs(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data = iris,
      main = "scatter plot",col=cols)

corrgram(iris[1:4],lower.panel=panel.conf,upper.panel=panel.pts,diag.panel=panel.density)
cor(iris[1:4])
corrplot(corr =cor(iris[1:4]),order="AOE",type="upper",tl.pos="tp")
corrplot(corr=cor(iris[1:4]),add=TRUE, type="lower", method="number",order="AOE", col="black",diag=FALSE,tl.pos="n", cl.pos="n")


colors=colors()[as.numeric(iris$Species)*11]
parcoord(iris[,c(1:4)] , col= colors)

hexbinplot(iris$Petal.Length~iris$Petal.Width,data=iris)

stars(iris[1:4], key.loc = c(14, 2), full = FALSE)
