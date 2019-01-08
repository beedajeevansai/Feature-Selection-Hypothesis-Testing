
library(data.table)
data <- fread("default of credit card clients")
data <- fread("default data.csv")

head(data, 5)
dim(data)
sum(is.na(data))

#parametric bootstrap

# zero_duly
a<- 1
result <- vector()
diff_vec <- vector()

n <- nrow(data)
p_hat_0 <- mean(data[data$zero_duly ==0]$`default payment next month`)            
p_hat_1 <- mean(data[data$zero_duly ==1]$`default payment next month`)            

p_hat_diff <- p_hat_1 - p_hat_0
diff_vec[a] <- p_hat_diff

# Bootstrap 1
theta.hat_Pbootstrap_0 = vector()

for(i in 1:3000){
  X_1 = sample(c(1, 0), n, replace = TRUE, prob = c(p_hat_0, 1 - p_hat_0))
  theta.hat_Pbootstrap_0[i]= mean(X_1)
}

# Bootstrap 2
theta.hat_Pbootstrap_1 = vector()

for(i in 1:3000){
  X_2 = sample(c(1, 0), n, replace = TRUE, prob = c(p_hat_1, 1 - p_hat_1))
  theta.hat_Pbootstrap_1[i]= mean(X_2)
}

theta.hat_Pbootstrap_diff <- theta.hat_Pbootstrap_1 - theta.hat_Pbootstrap_0

theta.hat_Pbootstrap_se = sd(theta.hat_Pbootstrap_diff)

Pbootstrap_CI <- c(p_hat_diff - 2*theta.hat_Pbootstrap_se, p_hat_diff + 2*theta.hat_Pbootstrap_se)
Pbootstrap_CI

if (Pbootstrap_CI[1]<= 0 &  Pbootstrap_CI[2] >= 0){
    result[a] <- 1
  } else {result[a] <- 0}

result[a]
a <- a + 1

# 0

--------------------------------------------------------
  
  # Atleast 1 duly
  
p_hat_0 <- mean(data[data$`Atleast 1 duly` ==0]$`default payment next month`)            
p_hat_1 <- mean(data[data$`Atleast 1 duly` ==1]$`default payment next month`)            

p_hat_diff <- p_hat_1 - p_hat_0
diff_vec[a] <- p_hat_diff

# Bootstrap 1
theta.hat_Pbootstrap_0 = vector()

for(i in 1:3000){
  X_1 = sample(c(1, 0), n, replace = TRUE, prob = c(p_hat_0, 1 - p_hat_0))
  theta.hat_Pbootstrap_0[i]= mean(X_1)
}

# Bootstrap 2
theta.hat_Pbootstrap_1 = vector()

for(i in 1:3000){
  X_2 = sample(c(1, 0), n, replace = TRUE, prob = c(p_hat_1, 1 - p_hat_1))
  theta.hat_Pbootstrap_1[i]= mean(X_2)
}

theta.hat_Pbootstrap_diff <- theta.hat_Pbootstrap_1 - theta.hat_Pbootstrap_0

theta.hat_Pbootstrap_se = sd(theta.hat_Pbootstrap_diff)

Pbootstrap_CI <- c(p_hat_diff - 2*theta.hat_Pbootstrap_se, p_hat_diff + 2*theta.hat_Pbootstrap_se)
Pbootstrap_CI

if (Pbootstrap_CI[1]<= 0 &  Pbootstrap_CI[2] >= 0){
  result[a] <- 1
} else {result[a] <- 0}

result[a]
a <- a + 1

--------------------------------------------------------
  
  # Atleast 2 duly
  
p_hat_0 <- mean(data[data$`Atleast 2 duly` ==0]$`default payment next month`)            
p_hat_1 <- mean(data[data$`Atleast 2 duly` ==1]$`default payment next month`)            

p_hat_diff <- p_hat_1 - p_hat_0
diff_vec[a] <- p_hat_diff

# Bootstrap 1
theta.hat_Pbootstrap_0 = vector()

for(i in 1:3000){
  X_1 = sample(c(1, 0), n, replace = TRUE, prob = c(p_hat_0, 1 - p_hat_0))
  theta.hat_Pbootstrap_0[i]= mean(X_1)
}

# Bootstrap 2
theta.hat_Pbootstrap_1 = vector()

for(i in 1:3000){
  X_2 = sample(c(1, 0), n, replace = TRUE, prob = c(p_hat_1, 1 - p_hat_1))
  theta.hat_Pbootstrap_1[i]= mean(X_2)
}

theta.hat_Pbootstrap_diff <- theta.hat_Pbootstrap_1 - theta.hat_Pbootstrap_0

theta.hat_Pbootstrap_se = sd(theta.hat_Pbootstrap_diff)

Pbootstrap_CI <- c(p_hat_diff - 2*theta.hat_Pbootstrap_se, p_hat_diff + 2*theta.hat_Pbootstrap_se)
Pbootstrap_CI

if (Pbootstrap_CI[1]<= 0 &  Pbootstrap_CI[2] >= 0){
  result[a] <- 1
} else {result[a] <- 0}

result[a]
a <- a + 1

--------------------------------------------------------
  
# Atleast 3 duly
  
p_hat_0 <- mean(data[data$`Atleast 3 duly` ==0]$`default payment next month`)            
p_hat_1 <- mean(data[data$`Atleast 3 duly` ==1]$`default payment next month`)            

p_hat_diff <- p_hat_1 - p_hat_0
diff_vec[a] <- p_hat_diff

# Bootstrap 1
theta.hat_Pbootstrap_0 = vector()

for(i in 1:3000){
  X_1 = sample(c(1, 0), n, replace = TRUE, prob = c(p_hat_0, 1 - p_hat_0))
  theta.hat_Pbootstrap_0[i]= mean(X_1)
}

# Bootstrap 2
theta.hat_Pbootstrap_1 = vector()

for(i in 1:3000){
  X_2 = sample(c(1, 0), n, replace = TRUE, prob = c(p_hat_1, 1 - p_hat_1))
  theta.hat_Pbootstrap_1[i]= mean(X_2)
}

theta.hat_Pbootstrap_diff <- theta.hat_Pbootstrap_1 - theta.hat_Pbootstrap_0

theta.hat_Pbootstrap_se = sd(theta.hat_Pbootstrap_diff)

Pbootstrap_CI <- c(p_hat_diff - 2*theta.hat_Pbootstrap_se, p_hat_diff + 2*theta.hat_Pbootstrap_se)
Pbootstrap_CI

if (Pbootstrap_CI[1]<= 0 &  Pbootstrap_CI[2] >= 0){
  result[a] <- 1
} else {result[a] <- 0}

result[a]
a <- a + 1

--------------------------------------------------------
  
  # Atleast 4 duly
  
p_hat_0 <- mean(data[data$`Atleast 4 duly` ==0]$`default payment next month`)            
p_hat_1 <- mean(data[data$`Atleast 4 duly` ==1]$`default payment next month`)            

p_hat_diff <- p_hat_1 - p_hat_0
diff_vec[a] <- p_hat_diff


# Bootstrap 1
theta.hat_Pbootstrap_0 = vector()

for(i in 1:3000){
  X_1 = sample(c(1, 0), n, replace = TRUE, prob = c(p_hat_0, 1 - p_hat_0))
  theta.hat_Pbootstrap_0[i]= mean(X_1)
}

# Bootstrap 2
theta.hat_Pbootstrap_1 = vector()

for(i in 1:3000){
  X_2 = sample(c(1, 0), n, replace = TRUE, prob = c(p_hat_1, 1 - p_hat_1))
  theta.hat_Pbootstrap_1[i]= mean(X_2)
}

theta.hat_Pbootstrap_diff <- theta.hat_Pbootstrap_1 - theta.hat_Pbootstrap_0

theta.hat_Pbootstrap_se = sd(theta.hat_Pbootstrap_diff)

Pbootstrap_CI <- c(p_hat_diff - 2*theta.hat_Pbootstrap_se, p_hat_diff + 2*theta.hat_Pbootstrap_se)
Pbootstrap_CI

if (Pbootstrap_CI[1]<= 0 &  Pbootstrap_CI[2] >= 0){
  result[a] <- 1
} else {result[a] <- 0}

result[a]
a <- a + 1


--------------------------------------------------------
  
  # Atleast 5 duly
  
  p_hat_0 <- mean(data[data$`Atleast 5 duly` ==0]$`default payment next month`)            
p_hat_1 <- mean(data[data$`Atleast 5 duly` ==1]$`default payment next month`)            

p_hat_diff <- p_hat_1 - p_hat_0
diff_vec[a] <- p_hat_diff

# Bootstrap 1
theta.hat_Pbootstrap_0 = vector()

for(i in 1:3000){
  X_1 = sample(c(1, 0), n, replace = TRUE, prob = c(p_hat_0, 1 - p_hat_0))
  theta.hat_Pbootstrap_0[i]= mean(X_1)
}

# Bootstrap 2
theta.hat_Pbootstrap_1 = vector()

for(i in 1:3000){
  X_2 = sample(c(1, 0), n, replace = TRUE, prob = c(p_hat_1, 1 - p_hat_1))
  theta.hat_Pbootstrap_1[i]= mean(X_2)
}

theta.hat_Pbootstrap_diff <- theta.hat_Pbootstrap_1 - theta.hat_Pbootstrap_0

theta.hat_Pbootstrap_se = sd(theta.hat_Pbootstrap_diff)

Pbootstrap_CI <- c(p_hat_diff - 2*theta.hat_Pbootstrap_se, p_hat_diff + 2*theta.hat_Pbootstrap_se)
Pbootstrap_CI

if (Pbootstrap_CI[1]<= 0 &  Pbootstrap_CI[2] >= 0){
  result[a] <- 1
} else {result[a] <- 0}

result[a]
a <- a + 1


--------------------------------------------------------
  
  # Atleast 6 duly
  
  p_hat_0 <- mean(data[data$`Atleast 6 duly` ==0]$`default payment next month`)            
p_hat_1 <- mean(data[data$`Atleast 6 duly` ==1]$`default payment next month`)            

p_hat_diff <- p_hat_1 - p_hat_0
diff_vec[a] <- p_hat_diff

# Bootstrap 1
theta.hat_Pbootstrap_0 = vector()

for(i in 1:3000){
  X_1 = sample(c(1, 0), n, replace = TRUE, prob = c(p_hat_0, 1 - p_hat_0))
  theta.hat_Pbootstrap_0[i]= mean(X_1)
}

# Bootstrap 2
theta.hat_Pbootstrap_1 = vector()

for(i in 1:3000){
  X_2 = sample(c(1, 0), n, replace = TRUE, prob = c(p_hat_1, 1 - p_hat_1))
  theta.hat_Pbootstrap_1[i]= mean(X_2)
}

theta.hat_Pbootstrap_diff <- theta.hat_Pbootstrap_1 - theta.hat_Pbootstrap_0

theta.hat_Pbootstrap_se = sd(theta.hat_Pbootstrap_diff)

Pbootstrap_CI <- c(p_hat_diff - 2*theta.hat_Pbootstrap_se, p_hat_diff + 2*theta.hat_Pbootstrap_se)
Pbootstrap_CI

if (Pbootstrap_CI[1]<= 0 &  Pbootstrap_CI[2] >= 0){
  result[a] <- 1
} else {result[a] <- 0}

result[a]
a <- a + 1

result

diff_vec



#X1 and X2 equal to their pi*n
x1 = p1.hat*n
x2 = p2.hat*n

#Sampling 10,000 random variables from the posterior distributions of pi1 and pi2
post1 = rbeta(10000,1+x1,1+n-x1)
post2 = rbeta(10000,1+x2,1+n-x2)

#Calulating posterior samples from pi1 - pi2
posterior = post1 - post2

#To find mean
mean(posterior)
# 0.105142

c(quantile(posterior, 0.025), quantile(posterior, 0.975))
