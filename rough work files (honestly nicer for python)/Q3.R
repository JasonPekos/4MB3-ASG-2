##Assumes that the great_plague.csv file is in the desktop
##setwd("Desktop)
df <- read.csv("great_plague.csv")


##question a)
## peak deaths is 48553, at a row index of 23.
## use that to end the fitting window for exponential growth.
## start at the first observation as it looks exponential there too.
##The fitting window is then [0,23]
df_fitting_window <- df[0:23,]
fit <- lm(data = df_fitting_window, formula = log(plague_deaths) ~ weeks)
##the current model is
## deaths(t) = deaths(0)*exp(r*t)
r <- coef(fit)[2]
dbl_time <- log(2)/r  ## doubling time

##question b)
##gamma is 1\mean infectious period, or 1/2.5
gamma <- 1/2.5
##beta is r + gamma
beta <- r + gamma
##R0 is beta/gamma
R0 <- beta/gamma

##For better fits.
adj_beta <- beta
adj_gamma <- gamma - 0.035

##question c)
library(deSolve)
##The population of London in 1665 was ~460,000. Will need to cite Britannica.
N = 460000
##Use the beta and gamma estimates from above
##Use the time from the original time series.
time <- df_fitting_window$weeks
pars <- c("beta" = adj_beta,
          "gamma" = adj_gamma,
          "N" = N)
inits <- c("S" = N,
           "I" = 1/(0.4*adj_beta),
           "R" = 0)
##Fix naming issue
names(inits) <- c("S", "I", "R")
SIR_model <- function(time, state, parms) {
  with(as.list(c(state, parms)), {
    dS <-  -beta/N*(N-(I))*(I)
    dI <-  beta/N*(N-(I))*(I) - gamma*I
    dR <- gamma * I
    return(list(c(dS, dI, dR)))
  })
}
sim <- as.data.frame(deSolve::ode(y = inits,
                           times = time,
                           func = SIR_model,
                           parms = pars))
##Add a predictions column that's incidence X CFR.
df_fitting_window$pred <- 0.4*sim$I
##Plot time series of deaths and superimose the solution of incidence * the case fatality ratio
matplot(x = time,
        y = df_fitting_window[,c("pred", "plague_deaths")],
        type="l",
        lty=1,
        main = "Predicted vs Reported deaths",
        xlab = "time (days)",
        ylab = "Daily proportion reported")
legend("topright",
       col = 1:2,
       legend=c("Predicted deaths", "Reported deaths"),
       lwd=1)

##Q1c)
x <- numeric(1000)
for (i in 1:length(x)){
  x[i] <- sum(rexp(rate = 1, 4))
}


hist(x, freq =  FALSE)
lines(dgamma(1:10, shape = 4, rate = 1))

