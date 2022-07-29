library(R6)
# ?R6Class

normal_model = R6Class(
  classname = "normal_model",
  public = list(y=NULL,mu=NULL,s2=NULL,tau_0=NULL,nu_0=NULL,y_mean=NULL,y_var=NULL,initial_mu=1,initial_s2=1,iteration=0,# what I need in self environment; initial them to null
                initialize = function(y,mu_0,s2_0,tau_0,nu_0,initial_mu=1,initial_s2=1) { # what I need to input
                  # This is "self" of initialize env.
                  self$y = y
                  self$y_mean = mean(y)
                  self$y_var = var(y)
                  self$mu = mu_0
                  self$initial_mu = initial_mu
                  self$s2 = s2_0
                  self$initial_s2 = initial_s2
                  self$tau_0 = tau_0
                  self$nu_0 = nu_0
                  self$iteration = 0
                },
                update = function(){
                  n = length(self$y)
                  mu = rnorm(1, 
                             mean = (((n * self$y_mean)/ self$s2) + (self$initial_mu/(self$tau_0^2))) / 
                               ((n/self$s2) + (1/(self$tau_0^2))),
                             sd = ((n/(self$s2) + 1/(self$tau_0^2)))^(-1/2))
                  s2 = (rgamma(1,
                               shape = (n + self$nu_0)/2,
                               rate = ((n-1) * (self$y_var) + 
                                         n * (self$y_mean - self$mu)^2 +
                                         self$nu_0 * self$initial_s2) /2))
                  self$mu = mu
                  self$s2 = s2
                  
                  self$iteration = self$iteration + 1
                  invisible(self)
                }
  ),
)


Y = c(24,43,58,71,43,49,61,44,67,49,53,56,59,52,62,54,57,33,46,43,57)
sampler <- normal_model$new(y=Y,mu_0=42,tau_0=200,s2_0=400,nu_0=1)
buffer_mu <- buffer_s2 <- numeric(1000)
set.seed(111)
for (i in 1:1000){
  sampler$update()
  buffer_mu[i] = sampler$mu
  buffer_s2[i] = sampler$s2
}

# check:
sampler$iteration

mean(buffer_mu)
sd(buffer_mu)

mean(buffer_s2)
sd(buffer_s2)

hist(buffer_mu)
hist(buffer_mu[2:1000])
hist(buffer_s2)

posterior_distribution_samples = cbind(buffer_mu,buffer_s2)
head(posterior_distribution_samples)
