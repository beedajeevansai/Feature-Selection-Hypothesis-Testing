library(bootstrap)

# Set working directory
setwd("C:\\Users\\jeeva\\Documents\\BANA\\Probability Models\\Project\\")

# Load dataset
credit_data <- read.csv("Credit_default.csv", header = TRUE, stringsAsFactors = FALSE)

# Creating additional columns
credit_data$BAL_AMT1 <- credit_data$BILL_AMT1 - credit_data$PAY_AMT1
credit_data$BAL_AMT2 <- credit_data$BILL_AMT2 - credit_data$PAY_AMT2
credit_data$BAL_AMT3 <- credit_data$BILL_AMT3 - credit_data$PAY_AMT3
credit_data$BAL_AMT4 <- credit_data$BILL_AMT4 - credit_data$PAY_AMT4
credit_data$BAL_AMT5 <- credit_data$BILL_AMT5 - credit_data$PAY_AMT5
credit_data$BAL_AMT6 <- credit_data$BILL_AMT6 - credit_data$PAY_AMT6

# Generating Non Parametric bootstrap function
theta_median <- function(x)
{
  median(x)
}

#Generating Bootstrap

results_pred1 <- apply(credit_data[which(credit_data$default.payment.next.month
                                          == 1), c(1,12:30)], 2, function(x) 
                                            bootstrap(x,nboot = 1000, theta_median))

theta_star1 <- sapply(results_pred1, `[[`, 1)

results_pred0 <- apply(credit_data[which(credit_data$default.payment.next.month
                                         == 0), c(1,12:30)], 2, function(x) 
                                           bootstrap(x,nboot = 1000, theta_median))

theta_star2 <- sapply(results_pred0, `[[`, 1)

theta_star_diff <- as.data.frame(theta_star1 - theta_star2)

se_median_diff <- apply(theta_star_diff, 2, function(x) sd(x))

median_hat1 <- apply(credit_data[which(credit_data$default.payment.next.month
                                         == 1), c(1,12:30)], 2, median)

median_hat0 <- apply(credit_data[which(credit_data$default.payment.next.month
                                         == 0), c(1,12:30)], 2, median)

median_hat_diff <- median_hat1 - median_hat0

output_df <- data.frame(median_hat_diff, se_median_diff,
                        median_hat_diff - 2*se_median_diff, median_hat_diff + 2*se_median_diff)

nrow(output_df)
names(output_df)[c(3,4)] <- c("lower_limit", "upper_limit")
length(median_hat_diff)
length(se_median_diff)

output_df$reject_null <- ifelse(output_df$lower_limit <= 0 & output_df$upper_limit >=0, 'NO', 'YES')

nrow(output_df[which(output_df$reject_null == 'YES'),])

write.csv(output_df[which(output_df$reject_null == 'YES'),], "Output_parametric.csv")

par(mfrow=c(1,3))
?hist

hist(credit_data$LIMIT_BAL, main = "Distribution of credit limit", xlab = "Credit Limit")

hist(credit_data$BILL_AMT1, main = "Distribution of bill amount", xlab = "Bill Amount1")

hist(credit_data$BAL_AMT1, main = "Distribution of bal amount", xlab = "Bal Amount1")
hist(credit_data$BILL_AMT1)

# Empirical cdf

#get ecdf
bal_ecdf <- ecdf(credit_data$LIMIT_BAL)

n=length(credit_data$LIMIT_BAL)
#calculate the upper and lower bands
grid=seq(10000,1000000 ,length.out=1000)
lower <- bal_ecdf(grid) - sqrt((1/n)*log(2/.05))
lower <- pmax(lower, 0)

upper <- bal_ecdf(grid) + sqrt((1/n)*log(2/.05))
upper <- pmin(upper, 1)

plot(bal_ecdf)
lines(grid, lower)
lines(grid, upper)





# Parametric bootstrap

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


# Bayesian Analysis

#X1 and X2 equal to their pi*n
p1.hat <- mean(data[data$`Atleast 6 duly` == 0]$`default payment next month`)            
p2.hat <- mean(data[data$`Atleast 6 duly` == 1]$`default payment next month`)            

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
