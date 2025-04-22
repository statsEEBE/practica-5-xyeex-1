#DISTRIBUCION POBLACIONAL
mu = 95.3 kg, sd = 5.7 kg
es distribueix de forma normal.
#a) seleccionar 4 cables, valor esperado de la suma de les resistencies de la mostra.
mux = n*mu, varx = var*sqrt(n)
mu<- 95.3
n<- 4
mux <- mu*n
mux
#b)seleccionar 100 cables, varianza de la suma de les resistencies de la mostra.
n <- 100
sd <- 5.7
sdx <- sqrt(n)*sd
varx<-sdx^2
varx
#simulacion para confirmar
set.seed(84)
n <- 100
Y <- function(i){sum(rnorm(n, 95.3, 5.7))}
Y(1)
sapply(1:10,Y)
simul100000<-sapply(1:100000,Y)



#c)seleccionar un cable, probabilidad de que la resisrtencias no sigui menor que 103 kg.
1-pnorm(103,mu,sd)
#d)seleccionar 4 cables, probabilidad de que la mitjana de les resistencias sigui menor que 98 kg.
n <- 4
mu <- 95.3
mux <- mu
mux
sd <- 5.7
sdx <- sd/sqrt(n)
varx <- sdx^2
sdx
X - N(95.3, 2.85)
pnorm(98, mux,sdx)

#simulacion para comprobar
#media de la media muestral para n = 4
set.seed(84)
n <- 4
x_bar <- function(i){sum(rnorm(n, 95.3, 5.7))}
simul100000<-sapply(1:100000,x_bar)
hist(simul100000)
simul100000
mean(simul100000)


#e)seleccionar 100 cables, probabilidad de que la varianza de les resistencias sigui mes gran que 32kg.
P(Ssq > 32)=P(a 32*(n-1)/sd^2) #a es una variable Chi cuatrado
n<- 100 una variable Chi cuatrado
n<- 100
sd <- 5.7
1-pchisq(32*(n-1)/sd^2, n-1)
#simulacion para comprobar
set.seed(84)
n <- 100
Ssq <- function(i){var(rnorm(n, 95.3, 5.7))}
Ssq(1)
simul100000<-sapply(1:100000,Ssq)
hist(simul100000)
simul100000
mean(simul100000)
simul100000>32
as.numeric(simul100000>32)
mean(as.numeric(simul100000>32))


#denorm = f(x), rnorm = simulaciones
curve(dnorm(x, 93.5, 5.7),xlim=c(80,120),
      col = "red")

set.seed(84)
n<- 4
simul <- rnorm(n, 95.3, 5.7)
simul
sum(simul)

#suma muestral
set.seed(84)
Y <- function(i){sum(rnorm(n, 95.3, 5.7))}
Y(1)
sapply(1:10,Y)
simul100000<-sapply(1:100000,Y)
hist(simul100000)

hist(simul100000,freq=FALSE)
n <- 4
mu <- 95.3
sigma <- 5.7
curve(dnorm(x,n*mu, sigma*sqrt(n)), col="red", add=TRUE)
mean(simul100000)
