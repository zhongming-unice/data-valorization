
set.seed(998)
users = sample(1:3,10,replace=T)

n = 10
m = 4
mu = 50
sigma = 10

C = matrix(0, nrow = 3, ncol = m)
for ( i in 1:3) { 
  C[i,] = rnorm(m, mean = mu, sd = sigma)
}

V = matrix(0, nrow = n, ncol = m)
for ( i in 1:n) { 
  t = users[i]
  V[i,] = C[t,] + rnorm(m, mean = 0, sd = 1)
}
V0 <- V
p = 0.1
for ( i in 1:n) {
  for (j in 1:m){
   if(runif(1)<p) {
     V[i,j] = NA
   }
  }
}

I = rep(0,n)
V_tile = rep(0,n)
for ( i in 1:n) { 
  x = V[i,]
  I[i] = length(x[!is.na(x)])
  x = na.omit(x)
  V_tile[i] = sum(x)/I[i]
}


W = matrix(0, nrow = n, ncol = n)
for (a in 1:n) {
  for (b in 1:n){
    q = 0
    w = 0
    e = 0
    for (j in 1:m){
      if (!is.na(V[a,j]) && !is.na(V[b,j])){
         q = q + (V[a,j]-V_tile[a]) * (V[b,j]-V_tile[b])
         w = w + (V[a,j]-V_tile[a])**2
         e = e + (V[b,j]-V_tile[b])**2
      }
      W[a,b] = q/sqrt(w*e)
    }
  }
}

gamma = rep(0,n)
for (i in 1:n){
  gamma[i] = 1/sum(abs(W[i,]))
}

V2 <- V

for (a in 1:n) {
    for (j in 1:m){
      if (is.na(V2[a,j])){
        S = 0
        for (b in 1:n) {
          if (!is.na(V2[b,j])){
            S = W[a,b]*(V2[b,j]-V_tile[b])
          }
        }
        V2[a,j] = V_tile[a] + gamma[a]*S
      }
    }
}
V0
V
V2
V0-V2