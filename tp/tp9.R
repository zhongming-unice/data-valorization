students <- read.table(file = "C:/Users/kamibukuro233/data-valorization/tp/studentStudy.txt",
                       row.names = 1,header = T,sep = "\t")
print(students)

class(students)
N <- 5
M <- 4
num_total <- sum(students)
table_fre <- students/num_total

table_fre_ind <- students
z <- students
R <- students
Q <- students

for (i in 1:N) {
  for (j in 1:M) {
    table_fre_ind[i,j] =  sum(students[i,])*sum(students[,j])/num_total^2
  }
}


for (i in 1:N) {
  for (j in 1:M) {
    a = sum(students[i,])*sum(students[,j])/num_total^2
    z[i,j] =  (table_fre[i,j]-a)/sqrt(a)
  }
}

phi = sqrt(sum(z^2))
chi = sqrt(num_total*phi^2)


# row
for (i in 1:N) {
  for (j in 1:M) {
    R[i,j] =  table_fre[i,j]/(sum(students[i,])*sqrt(sum(students[,j])))-sqrt(sum(students[,j]))
  }
}
V = t(as.matrix(z)) %*% as.matrix(z)

valeur_propre <- eigen(V)$values
vecteur_propre <- eigen(V)$vec

proportion = valeur_propre/sum(valeur_propre)*100

s1_row = rep(0,N)
s2_row = rep(0,N)
u1 = vecteur_propre[,1]
u2 = vecteur_propre[,2]

for (i in 1:N) {
  for (j in 1:M) {
    s1_row[i] = s1_row[i] + R[i,j]*u1[j]
    s2_row[i] = s2_row[i] + R[i,j]*u2[j]
  }
}

# colum
for (i in 1:N) {
  for (j in 1:M) {
    Q[i,j] =  table_fre[i,j]/(sum(students[,j])*sqrt(sum(students[i,])))-sqrt(sum(students[i,]))
  }
}
W = as.matrix(z) %*% t(as.matrix(z))
valeur_propre2 <- eigen(W)$values
vecteur_propre2 <- eigen(W)$vec

proportion2 = valeur_propre2/sum(valeur_propre2)*100

s1_col = rep(0,N)
s2_col = rep(0,N)
v1 = vecteur_propre[,1]
v2 = vecteur_propre[,2]

for (i in 1:N) {
  for (j in 1:M) {
    s1_col[i] = s1_col[i] + Q[i,j]*v1[j]
    s2_col[i] = s2_col[i] + Q[i,j]*v2[j]
  }
}

plot(s1_row,s2_row)
plot(s1_col,s2_col)






library('FactoMineR')
res <- CA(students)
summary(res)