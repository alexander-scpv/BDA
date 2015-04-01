library(rjags)

# The model specification
model_string <- "model {
tau ~ dgamma(20,2000)
sigma <- 1/sqrt(tau)
theta ~ dnorm(5,0.25)
n <- 2*pow((1.28 + 1.96)*sigma/theta,2)
power <- phi(sqrt(84/2)*theta/sigma - 1.96)
x <- power - 0.7
p70 <- step(x)
}"

# Running the model
model <- jags.model(textConnection(model_string), n.chains = 1, n.adapt= 10000)
update(model, 10000); # Burnin for 1000 samples
mcmc_samples <- coda.samples(model, variable.names=c("n","p70", "power"), n.iter=20000)
plot(mcmc_samples)
summary(mcmc_samples)

x1 <- mcmc_samples[,1, drop=FALSE]
x2 <- mcmc_samples[,2, drop=FALSE] 
x3 <- mcmc_samples[,3, drop=FALSE]
print(x3) 
lapply(x2, write, "testBugs.txt", append=TRUE, ncolumns=1)
#write.table(x2, file = "dataBugsBookExample.p70")

