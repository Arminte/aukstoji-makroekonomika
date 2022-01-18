g <- 1.01
phi <- g - 1
phi
1/phi
lambda <- (1 - phi^0.5)/(1 + phi^0.5)
lambda
#gamybos reakcija
round(0.5*(1 + lambda)*lambda^(0:10), 4)
#kainu reakcija
round(0.5*(1 - lambda)*lambda^(0:10), 4)

g <- 1.25
phi <- g - 1
phi
1/phi
lambda <- (1 - phi^0.5)/(1 + phi^0.5)
lambda
round(0.5*(1 + lambda)*lambda^(0:10), 4)
round(0.5*(1 - lambda)*lambda^(0:10), 4)

g <- 1.5
phi <- g - 1
phi
1/phi
lambda <- (1 - phi^0.5)/(1 + phi^0.5)
round(0.5*(1 + lambda)*lambda^(0:10), 4)
round(0.5*(1 - lambda)*lambda^(0:10), 4)

g <- 1.75
phi <- g - 1
phi
1/phi
lambda <- (1 - phi^0.5)/(1 + phi^0.5)
lambda
round(0.5*(1 + lambda)*lambda^(0:10), 4)
round(0.5*(1 - lambda)*lambda^(0:10), 4)

g <- 1.99
phi <- g - 1
phi
1/phi
lambda <- (1 - phi^0.5)/(1 + phi^0.5)
lambda
round(0.5*(1 + lambda)*lambda^(0:10), 4)
round(0.5*(1 - lambda)*lambda^(0:10), 4)
