### Statistical analyses

#### Investigating outcome variable
dat<-read.table("data_for_prediction.txt", header = TRUE, sep = "\t", dec = ".")

data <- read.table("data_tr.txt", header = TRUE, sep = "\t", dec = ".")[,-1]
y <- data$tw
summary(y)

hist(y)

#### Ordinary Least Square Regression
reg1 <- lm(y ~ age + marr  + male + educ, data = data)
plot(reg1)
summary(reg1)
reg2 <- lm(y ~ e401 + ira + nifa + inc + hequity, data = data)
summary(reg2)
plot(reg2)
reg3 <- lm(y ~ e401 + ira + nifa + inc + hequity + age + male + educ + marr, data = data)
summary(reg3)

##### Compute the in sample MSPE of each model
MSPE_reg1 <- mean((predict(reg1) - y)^2)
MSPE_reg2 <- mean((predict(reg2) - y)^2)
MSPE_reg3 <- mean((predict(reg3) - y)^2)

c(MSPE_reg1, MSPE_reg2, MSPE_reg3)

##### 5-fold cross validation to compare the three ols models
n <- nrow(y)
k <- 5
set.seed(123)
ii <- sample(rep(1:k, length= n))
mspe1 <- mspe2 <- mspe3 <- vector(length = k)

for (j in 1:k){
  
  hold <- (ii == j)
  train <- (ii != j)
  y1 <- data$tw[hold]
  
  reg1 <- lm(tw ~ age + marr  + male + educ, data = data[train,])
  reg2 <- lm(tw ~ e401 + ira + nifa + inc + hequity, data = data[train,])
  reg3 <- lm(tw ~ e401 + ira + nifa + inc + hequity + age + male + educ + marr, data = data[train,])
  
  pr1 <- predict(reg1,newdata = data[hold,])
  pr2 <- predict(reg2,newdata = data[hold,])
  pr3 <- predict(reg3,newdata = data[hold,])
  
  mspe1 <- mean((y1 - pr1)^2)
  mspe2 <- mean((y1 - pr2)^2)
  mspe3 <- mean((y1 - pr3)^2)
}

c(mspe1, mspe2, mspe3)

#### Stepwise,Lasso, Ridge

##### 5-fold cross validation to compare stepwise, lasso, ridge
library(MASS)
library(glmnet)
n <- length(y)
k <- 5
ii <- sample(rep(1:k, length= n))
pr.stepwise_backward <- pr.stepwise_forward <- pr.lasso <- pr.ridge <- rep(NA, length(y))

for (j in 1:k){
  
  hold <- (ii == j)
  train <- (ii != j)
  ## Stepwise 
  full <- lm(tw ~ ., data=data[train,])
  null <- lm(tw ~ 1, data=data[train,])
  a <- stepAIC(null, scope=list(lower=null, upper=full), trace = FALSE, direction='forward')
  # backward stepwise - AIC
  b <- stepAIC(full, scope=list(lower=null, upper=full), trace = FALSE, direction='backward')
  pr.stepwise_backward[hold] <- predict(b, newdata=data[hold,])
  pr.stepwise_forward[hold] <- predict(a, newdata=data[hold,])
  
  ## Do with lasso (we use X and y defined above)
  xx.tr <- data[train,-1]
  y.tr <-  y[train]
  xx.te <- data[hold,-1]
  ridge.cv <- cv.glmnet(x=as.matrix(xx.tr), y=y.tr, nfolds=k, alpha=0)
  lasso.cv <- cv.glmnet(x=as.matrix(xx.tr), y=y.tr, nfolds=k, alpha=1)
  pr.lasso[hold] <- predict(lasso.cv, newx=as.matrix(xx.te))
  pr.ridge[hold] <- predict(ridge.cv, newx=as.matrix(xx.te))
}

mspe_step_backward <- mean((pr.stepwise_backward-y)^2)
mspe_step_forward <- mean((pr.stepwise_forward-y)^2)
mspe.Lasso <- mean((pr.lasso-y)^2)
mspe.ridge <- mean((pr.ridge-y)^2)

c(mspe_step_backward, mspe_step_forward, mspe.Lasso, mspe.ridge)
null <- lm(tw ~ 1, data = data)
full <- lm(tw ~., data = data)
step_model <- stepAIC(full, scope=list(lower=null, upper=full), trace = FALSE, direction='backward')

step_model
mspe_step_model <- mean((predict(step_model) - y)^2)
mspe_step_model

#### Removing outliers from the data set
quantile(data$tw, 0.99)

data_trimmed <- subset(data, data$tw < quantile(data$tw, 0.99))

yb <- data_trimmed$tw 

reg1b <- lm(yb ~ age + marr  + male + educ, data = data_trimmed)
reg2b <- lm(yb ~ e401 + ira + nifa + inc + hequity, data = data_trimmed)
reg3b <- lm(yb ~ e401 + ira + nifa + inc + hequity + age + male + educ + marr, data = data_trimmed)
plot(reg1b)
plot(reg2b)
plot(reg3b)
summary(reg1b)
summary(reg2b)
summary(reg3b)

#### Compute the in sample MSPE of each mod
MSPE_reg1b <- mean((predict(reg1b) - yb)^2)
MSPE_reg2b <- mean((predict(reg2b) - yb)^2)
MSPE_reg3b <- mean((predict(reg3b) - yb)^2)

c(MSPE_reg1b, MSPE_reg2b, MSPE_reg3b)

#### 5-fold cross validation to compare the three ols models 
n <- nrow(yb)
k <- 5
set.seed(123)
ii <- sample(rep(1:k, length= n))
mspe1b <- mspe2b <- mspe3b <- vector(length = k)

for (j in 1:k){
  
  hold <- (ii == j)
  train <- (ii != j)
  yb2 <- data_trimmed$tw[hold]
  
  reg1b2 <- lm(tw ~ age + marr  + male + educ, data = data_trimmed[train,])
  reg2b2 <- lm(tw ~ e401 + ira + nifa + inc + hequity, data = data_trimmed[train,])
  reg3b2 <- lm(tw ~ e401 + ira + nifa + inc + hequity + age + male + educ + marr, data = data_trimmed[train,])
  
  pr1b <- predict(reg1b2,newdata = data_trimmed[hold,])
  pr2b <- predict(reg2b2,newdata = data_trimmed[hold,])
  pr3b <- predict(reg3b2,newdata = data_trimmed[hold,])
  
  mspe1b <- mean((yb2 - pr1b)^2)
  mspe2b <- mean((yb2 - pr2b)^2)
  mspe3b <- mean((yb2 - pr3b)^2)
}

c(mspe1b, mspe2b, mspe3b)

##### Cross validation to compare stepwise, lasso, ridge
n <- length(yb)
k <- 5
ii <- sample(rep(1:k, length= n))
pr.stepwise_backward <- pr.stepwise_forward <- pr.lasso <- pr.ridge <- rep(NA, length(y))

for (j in 1:k){
  
  hold <- (ii == j)
  train <- (ii != j)
  ## Stepwise 
  full <- lm(tw ~ ., data=data_trimmed[train,])
  null <- lm(tw ~ 1, data=data_trimmed[train,])
  a <- stepAIC(null, scope=list(lower=null, upper=full), trace = FALSE, direction='forward')
  # backward stepwise - AIC
  b <- stepAIC(full, scope=list(lower=null, upper=full), trace = FALSE, direction='backward')
  pr.stepwise_backward[hold] <- predict(b, newdata=data_trimmed[hold,])
  pr.stepwise_forward[hold] <- predict(a, newdata=data_trimmed[hold,])
  
  ## Do with lasso (we use X and y defined above)
  xx.tr <- data_trimmed[train,-1]
  y.tr <-  yb[train]
  xx.te <- data_trimmed[hold,-1]
  ridge.cv <- cv.glmnet(x=as.matrix(xx.tr), y=y.tr, nfolds=k, alpha=0)
  lasso.cv <- cv.glmnet(x=as.matrix(xx.tr), y=y.tr, nfolds=k, alpha=1)
  pr.lasso[hold] <- predict(lasso.cv, newx=as.matrix(xx.te))
  pr.ridge[hold] <- predict(ridge.cv, newx=as.matrix(xx.te))
}

mspe_step_backwardb <- mean((pr.stepwise_backward-yb)^2)
mspe_step_forwardb <- mean((pr.stepwise_forward-yb)^2)
mspe.Lassob <- mean((pr.lasso-yb)^2)
mspe.ridgeb <- mean((pr.ridge-yb)^2)

c(mspe_step_backwardb, mspe_step_forwardb, mspe.Lassob, mspe.ridgeb)

#stepwise backwards gives us the most optimized model

null <- lm(tw ~ 1, data = data_trimmed)
full <- lm(tw ~., data = data_trimmed)
step_modelb <- stepAIC(full, scope=list(lower=null, upper=full), trace = FALSE, direction='backward')

step_modelb
mspe_step_modelb <- mean((predict(step_modelb) - yb)^2)
mspe_step_modelb
