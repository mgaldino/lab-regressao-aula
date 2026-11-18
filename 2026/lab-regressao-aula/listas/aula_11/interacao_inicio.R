# Cada linha representa uma seção. A numeração das zonas recomeça em cada estado.
set.seed(6183)
secoes <- expand.grid(secao = 1:3, zona = 1:20, D = 0:1)
secoes$estado <- factor(secoes$D, levels = 0:1, labels = c("RJ", "SP"))
secoes$id_zona <- with(secoes, interaction(estado, zona, drop = TRUE))
secoes$p <- with(secoes, 17 + 2 * zona + secao)
secoes$x <- secoes$p - mean(secoes$p)

# As três seções de cada zona compartilham um choque.
choque_zona <- rnorm(nlevels(secoes$id_zona), sd = 3)
erro_secao <- rnorm(nrow(secoes), sd = 1 + (60 - secoes$p) / 8)
secoes$Y <- with(secoes, 49 + 0.80*x + 5*D - 0.30*x*D +
                   choque_zona[id_zona] + erro_secao)
