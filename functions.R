# Annuities ----

discountRate <- function(i){
  1 / (1 + i)
}

annualEffective <- function(i){
  i / (i + 1)
}

forceOfInterest <- function(i){
  -log(1 - annualEffective(i))
}

interestPthly <- function(i,p=1){
  p * ((1+i)^(1/p) - 1)
}

effectivePthly <- function(i,p=1){
  p * (1-(1-annualEffective(i))^(1/p))
}

# Loss Distributions ----

## Normal Distribution ----


normplot <- function(x, mean,variance){
  m <- mean
  sd <- sqrt(variance)
  
  dnormx <- function(x){
    return(dnorm(x,m,sd))
  }
  
  ggplot(NULL, aes(c(-4*sd+m,4*sd+m))) +
    geom_area(stat = "function", fun = dnormx, fill = "turquoise", xlim = c(-4*sd+m,4*sd+m))+
    geom_area(stat = "function", fun = dnormx, fill = "red", xlim = c(-4*sd+m,x))+
    geom_line(stat = "function", fun = dnormx, size = 0.6,  xlim = c(-4*sd+m,4*sd+m))+
    labs(x="",y="") 
}

## Binomial Distribution ----

binplot <- function(x,n,p){
  data <- dbinom(x=0:n,n,p)
  names(data) <- 0:n
  columns <- rep("turquoise", n + 1)
  columns[x+1] <- "red"
  columns[0:x] <- "indianred1"
  barplot(data, col = columns)
}

## Gamma Distribution----

gammaplot <- function(x,a,b){
  if(x<=0 |a <=0 | b <=0) stop("Parameters must be greater than 0")
  upper <- 3*(a-1)+(9/b)
  
  dgammax <- function(x){
    return(dgamma(x,a,b))
  }
  
  ggplot(NULL, aes(c(0,upper))) +
    geom_area(stat = "function", fun = dgammax, fill = "turquoise", xlim = c(0,upper))+
    geom_area(stat = "function", fun = dgammax, fill = "red", xlim = c(0,x))+
    geom_line(stat = "function", fun = dgammax, size = 0.6, xlim = c(0,upper))+
    labs(x="",y="")
  
}

## Poisson Distribution----

poplot <- function(x,k){
  n = 8 + (k-1)*2
  data <- dpois(x=0:n,k)
  names(data) <- 0:n
  columns <- rep("turquoise", n + 1)
  columns[x+1] <- "red"
  columns[0:x] <- "indianred1"
  barplot(data, col = columns)
}

## Pareto Distribution----

pareto2plot <- function(x,a,k){
  if(x <= 0 | a <= 0 | k <= 0) stop("Parameters must be greater than 0")
  
  upper <- (9+k)/a
  
  dpareto2x <- function(x){
    return(dpareto2(x,a,k))
  }
  
  ggplot(NULL, aes(c(0,upper))) +
    geom_area(stat = "function", fun = dpareto2x, fill = "turquoise", xlim = c(0,upper))+
    geom_area(stat = "function", fun = dpareto2x, fill = "red", xlim = c(0,x))+
    geom_line(stat = "function", fun = dpareto2x, size = 0.6, xlim = c(0,upper))+
    labs(x="",y="")
}

pareto2Mean <- function(a,k){
  if(a <= 1) stop("alpha must be greater than 1")
  
  k/(a-1)
}

pareto2Var <- function(a,k){
  if(a <= 2) stop("alpha must be greater than 2")
  
  (a*k^2)/((a-1)^2 * (a-2))
}

