library(MASS)
library(nloptr)

# Joint prices
#Oil
dat1 = read.csv(file.choose())
#FX
dat2 = read.csv(file.choose())

# Marinal Prices
dat_oil = read.csv(file.choose())
dat_fx = read.csv(file.choose())

################################# Single Distibution Estimatin ##########################

# Pricing function Calculation
# Function for Integrad
pricing_fun1 = function(x,k,p1,p2)
{
  dnorm(x,mean = p1, sd = p2)*(x-k)
}

# Integral
pricing_fun2 = function(k,p1,p2)
{
  dummy_fun = function(x)
    pricing_fun1(x,k,p1,p2)
  integrate(dummy_fun, k, Inf)$value
}

# OBjective Function - Total SSE
min.RSS = function(par)
{
  a = 0
  for (i in 1:nrow(dat))
    a = a + ((pricing_fun2(dat$Strike[i],par[1],par[2])-dat$Price[i]))^2
  a
}

# Optimizattion Routine - BOBYQA
# Choose proper intialization point at x0
res0 <- nloptr( x0=c(55,10),
                eval_f=min.RSS,
                lb = c(0,0),
                ub = c(Inf,Inf),
                opts = list("algorithm"="NLOPT_LN_BOBYQA", maxeval = 100000, print_level = 1)
)
print(res0)


###################### Joint Distribution Estimation #####################

# First Integrand for Q1
pricing_fun3a = function(x,p1,p2)
{
  dnorm(x, mean = p1, sd = p2)*x
  
}

# Second Integrand for Q1
pricing_fun3b = function(x,k2,p3,p4)
{
  dnorm(x, mean = p3, sd = p4)*(x-k2)
  
}

# First Integrand for Q2
pricing_fun4a = function(x,k1,p1,p2)
{
  dnorm(x, mean = p1, sd = p2)*(x-k1)
  
}

# Second Integrand for Q2 
pricing_fun4b = function(x,p3,p4)
{
  dnorm(x, mean = p3, sd = p4)*x
  
}

# Integral for Q1

pricing_fun2a_1 = function(p1,p2)
{
  dummy_fun = function(x)
    pricing_fun3a(x,p1,p2)
  integrate(dummy_fun, 0, Inf)$value
}
pricing_fun2a_2 = function(k2, p3, p4)
{
  dummy_fun = function(x)
    pricing_fun3b(x,k2,p3,p4)
  integrate(dummy_fun, k2, Inf)$value
}

# Integral for Q2

pricing_fun2b_1 = function(k1,p1,p2)
{
  dummy_fun = function(x)
    pricing_fun4a(x,k1,p1,p2)
  integrate(dummy_fun, k1, Inf)$value
}
pricing_fun2b_2 = function(p3, p4)
{
  dummy_fun = function(x)
    pricing_fun4b(x,p3,p4)
  integrate(dummy_fun, 0, Inf)$value
}

# Objctive Function

# Error in estimating Q1
min.RSS1 = function(par)
{
  a = 0
  for (i in 1:nrow(dat2))
    a = a + ((pricing_fun2a_1(par[1],par[2])*pricing_fun2a_2(dat2$Strike[i],par[3],par[4]))-dat1$Price[i])^2
  a
}

# Error in estimating Q2
min.RSS2 = function(par)
{
  a = 0
  for (i in 1:nrow(dat1))
    a = a + ((pricing_fun2b_1(dat1$Strike[i],par[1],par[2])*pricing_fun2b_2(par[3],par[4]))-dat2$Price[i])^2
  a
}

# Error in estimating Marginal Oil option price from problem 1 & 2
min.RSS_oil = function(par)
  {
     a = 0
    for (i in 1:nrow(dat_oil))
     a = a + ((pricing_fun2(dat_oil$Strike[i],par[1],par[2])-dat_oil$Price[i]))^2
     a
  }

# Error in estimating Marginal FX option price from problem 1 & 2
min.RSS_fx = function(par)
{
  a = 0
  for (i in 1:nrow(dat_fx))
    a = a + ((pricing_fun2(dat_fx$Strike[i],par[3],par[4])-dat_fx$Price[i]))^2
  a
}

# Final Total SSE

min.RSS_final <- function(par){
  
  min.RSS1 + min.RSS2 + min.RSS_oil + min.RSS_fx}

min.RSS = function(par)
 {
   a = 0
   for (i in 1:nrow(dat))
     a = a + ((pricing_fun2(dat$Strike[i],par[1],par[2])-dat$Price[i]))^2
   a
 }

# Optimization Routine
# Input proper intialization
res0 <- nloptr( x0=c(40,10,40,10),
                eval_f=min.RSS,
                lb = c(0,0,0,0),
                ub = c(Inf,Inf, Inf,Inf),
                opts = list("algorithm"="NLOPT_LN_BOBYQA", maxeval = 100000, print_level = 1)
               )
print(res0)


##### Sample Output file generation ############

pricing_fun_out1 = function(x,strike,mean,sd)
{
  dnorm(x, mean = mean, sd = sd)*(x-strike)
  
}


pricing_fun_out2 = function(strike,mean,sd)
{
  dummy_fun = function(x)
    pricing_fun_out1(x,strike,mean,sd)
  integrate(dummy_fun, strike, Inf)$value
}

# Optimal value included 
for(i in 1:nrow(Oil_FX_joint)){
Oil_FX_joint$Price[i] = pricing_fun_out2(Oil_FX_joint$B_1[i], 54.22097, 19.95393)*pricing_fun_out2(Oil_FX_joint$B_2[i], 45.27257, 9.016578)
}

########### Some codes for plotting ###############

x <- seq(0, 100, length = 10000)
hx <- dnorm(x = x, mean = 50.1165, sd = 7.00022)
#hx <- dgamma(x = x, shape = 0.7024, scale = 13.52369)
plot(x,hx, main = "Price distribution for Oil")



library(MASS)
bivn <- mvrnorm(10000, mu = c(54.21708, 45.27), Sigma = matrix(c(398.0025, 0, 0, 81.28826), 2))

# now we do a kernel density estimate
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)


contour(bivn.kde)
image(bivn.kde)
persp(bivn.kde, phi = 45, theta = 30)


image(bivn.kde); contour(bivn.kde, add = T)
persp(bivn.kde, phi = 45, theta = 30, shade = .1, border = NA)





