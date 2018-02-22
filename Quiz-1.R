##########################################
# CISC - 863 Statistical Machine Learning
# Quiz 2
# Rohit Ravishankar
# rr9105@rit.edu
##########################################

# Graphing packages
library(ggplot2)
library(dplyr)

Theta <- seq( -3 , 3 , length=20)
Prior <- dnorm(Theta,0,6)
data <- data.frame(Theta, Prior)

ggplot(data, aes(Theta)) + 
  geom_line(aes(y = Prior, color = "blue")) +
  theme_minimal()+
  ylab("Prior")+
  xlab("Theta")

D <- c(3.2877564, 3.3049376, 3.0065537, 2.4179332, 2.4255153, 3.3632328,
       3.3750101, 3.6351869, 2.5573695, 2.4708438, 2.7914828, 2.8363451,
       3.5001336, 2.7917707, 2.3019892,2.8733925, 2.4083626, 2.9122397,
       3.1874000, 3.3319748)
m <- mean(D)
N <- length(D)
v <- 10
y <- (D-m)^2
a1 <- (2*pi*v^2)^-(N/2)
a2 <- (-1/(2*v^2))
likelihood <- a1*exp(a2*sum(y))

data$Likelihood = Likelihood



likelihoodFunction = function(theta, x) {
  mu = theta[1]
  sig2 = theta[2]
  n = length(x)
  a1 = (2*pi*sig^2)^-(N/2)
  a2 = -1/(2*sig2)
  y = (x-mu)^2
  ans = a1*exp(a2*sum(y))
  return(ans) 
}




  
  
