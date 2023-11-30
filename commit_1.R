# Meu primeiro commit

# matriz

# Exemplos
matrix(1:6, nrow=3, ncol=2)

x <- matrix(1:6, nrow=3, ncol=2)
x[2,1]

# data frame
# Exemplos
data.frame(x=1:3, y=c("a", "b", "c"))


## explicando data rapidamente
data_ex <- "2020-10-23"
data_ex1 <- as.Date(data_ex)

class(data_ex)
class(data_ex1)

data_ex1 + 0:9


## criando data de forma repetitiva e tediosa. O que queremos evitar!
minha_data <- c(as.Date('2009-01-01'), as.Date('2009-01-02'), as.Date('2009-01-03'),
                as.Date('2009-01-04'), as.Date('2009-01-05'), as.Date('2009-01-06'),
                as.Date('2009-01-07'), as.Date('2009-01-08'), as.Date('2009-01-09'),
                as.Date('2009-01-10'))

acoes <- data.frame(
  tempo = minha_data,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)

acoes


# criando data.frame de maneira mais inteligente
acoes <- data.frame(
  tempo = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)

acoes
