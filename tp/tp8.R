data("iris")

m <- naiveBayes(iris[,-5],iris[,5])

# X1,X2,X3,X4 = iris[,-5] = S.Length, S.Width, P.Length, P.Width
# C = iris[,5] = species

# A-priori probabilities:
# setosa versicolor  virginica 
# 0.3333333  0.3333333  0.3333333

# P(X=x_i|C=c_k) ~ N(mu_ik,sd_ik)

t <- table(predict(m,iris),iris[,5])
# Pr(Var2|Var1) = Freq/50

data("Titanic")

df <- as.data.frame(Titanic)

nb_passager <- sum(df$Freq)
nb_passager_vivant <- sum(df[df$Survived=='Yes',]$Freq)

prior_survived <- nb_passager_vivant/nb_passager
prior_Nosurvived <- 1 - prior_survived

dfrep <- data.frame(Class = rep(df$Class, df$Freq),
                    Sex = rep(df$Sex, df$Freq),
                    Age = rep(df$Age, df$Freq),
                    Survived = rep(df$Survived, df$Freq))

Monnaivebaye <- function(training, test){
  
}





m2 <- naiveBayes(dfrep[,-4],dfrep[,4])
t2 <- table(predict(m2,dfrep),dfrep[,4])
