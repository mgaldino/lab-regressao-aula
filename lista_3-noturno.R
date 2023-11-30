# Lista 3 - noturno

minha_unif <- runif(1000, min=0, max=10)
mean(minha_unif)

minha_media30 <- numeric()
minha_media50 <- numeric()
minha_media100 <- numeric()

for( i in 1:30) {
  minha_unif <- runif(1000, min=0, max=10)
  minha_media30[i] <- mean(minha_unif)
}
hist(minha_media30)


for( i in 1:50) {
  minha_unif <- runif(1000, min=0, max=10)
  minha_media50[i] <- mean(minha_unif)
}
hist(minha_media50)

for( i in 1:100) {
  minha_unif <- runif(1000, min=0, max=10)
  minha_media100[i] <- mean(minha_unif)
}
hist(minha_media100)

## questão 10

minha_norm <- rnorm(10)
minha_binom <- rbinom(20, size=1, p=.7)
