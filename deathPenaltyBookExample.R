library(rjags)

# The model specification


x1 <- c(34453,41534,35802,26954,31468,32552,40873,34861,42562,31900,37421,33305,32108,45844,34743,29709,36777)
x2 <- c(16.7,12.5,10.6,18.4,14.8,18.8,11.6,13.1,9.4,14.3,8.2,16.4,18.4,9.3,10,15.2,11.7)
x3 <- c(12.2,20,11.2,16.1,25.9,3.5,15.3,30.1,4.3,15.4,8.2,7.2,32.1,27.4,4,7.7,1.8)
x4 <- log(c(644,351,591,524,565,632,886,997,405,1051,537,321,929,931,435,597,463))
x5 <- c(1,1,0,1,1,0,0,1,0,1,0,0,1,0,0,0,0)
x6 <- c(0.16,0.27,0.21,0.16,0.19,0.25,0.25,0.21,0.31,0.24,0.19,0.16,0.18,0.29,0.24,0.21,0.25)

xm1 <- mean(x1)
xm2 <- mean(x2)
xm3 <- mean(x3)
xm4 <- mean(x4)
xm5 <- mean(x5)
xm6 <- mean(x6)

print(xm1)
print(xm2)
print(xm3)
print(xm4)
print(xm5)
print(xm6)

xsd1 <- sd(x1)
xsd2 <- sd(x2)
xsd3 <- sd(x3)
xsd4 <- sd(x4)
xsd5 <- sd(x5)
xsd6 <- sd(x6)

print(xsd1)
print(xsd2)
print(xsd3)
print(xsd4)
print(xsd5)
print(xsd6)

z1=vector(mode = "numeric", length = 17)
z2=vector(mode = "numeric", length = 17)
z3=vector(mode = "numeric", length = 17)
z4=vector(mode = "numeric", length = 17)
z5=vector(mode = "numeric", length = 17)
z6=vector(mode = "numeric", length = 17)
for (i in 1:17) {
z1[i] <- (x1[i]-xm1)
#/xsd1
z2[i] <- (x2[i]-xm2)
#/xsd2
z3[i] <- (x3[i]-xm3)
#/xsd3
z4[i] <- (x4[i]-xm4)
#/xsd4
z5[i] <- (x5[i]-xm5)
#/xsd5
z6[i] <- (x6[i]-xm6)
#/xsd6
}

print(z1)
print(z2)
print(z3)

beta0Init=10
beta1Init=30000
beta2Init=12
beta3Init=20
beta4Init=400
beta5Init=1
beta6Init=0.20

initsList <- list(beta0=beta0Init,beta1=beta1Init,beta2=beta2Init,
beta3=beta3Init,beta4=beta4Init,beta5=beta5Init,beta6=beta6Init)

model_string <- "
data {

y[1] <- 37
y[2] <- 9
y[3] <- 6
y[4] <- 4
y[5] <- 3
y[6] <- 2
y[7] <- 2
y[8] <- 2
y[9] <- 1
y[10] <- 1
y[11] <- 1
y[12] <- 1
y[13] <- 1
y[14] <- 1
y[15] <- 1
y[16] <- 1
y[17] <- 1
}

model {

for (i in 1:17) { 

  y[i] ~ dpois(mu[i])
  #y.pred[i] ~ dpois(mu[i])
  log(mu[i]) <- beta0+beta1*x1[i] + beta2*x2[i]  + beta3*x3[i] + beta4*x4[i]  + beta5*x5[i] + beta6*x6[i] 
#  log(mu[i]) <- beta0+beta1*z1[i] + beta2*z2[i]  + beta3*z3[i] + beta4*z4[i]  + beta5*z5[i] + beta6*z6[i]   
}


#total <- y.pred[1] + y.pred[2] + y.pred[3]
beta0 ~ dnorm(0,0.0001)
beta1 ~ dnorm(0,0.0001)
beta2 ~ dnorm(0,0.0001)
beta3 ~ dnorm(0,0.0001)
beta4 ~ dnorm(0,0.0001) 
beta5 ~ dnorm(0,0.0001)
beta6 ~ dnorm(0,0.0001)



}"

# Running the model
model <- jags.model(textConnection(model_string), n.chains = 1)
update(model, 1000); #1000 burnin
mcmc_samples <- coda.samples(model, variable.names=c("beta0","beta1","beta2","beta3","beta4","beta5","beta6"), n.iter=10000)
plot(mcmc_samples)
summary(mcmc_samples)



