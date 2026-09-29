# Gera os dados anuais da Lista 2 e confere os resultados dos exercícios.
# Executar da pasta 2026/lab-regressao-aula:
# Rscript --vanilla listas/lista_02/docente/validar_lista_02.R

stopifnot(file.exists("syllabus.Rmd"))
stopifnot(requireNamespace("dplyr", quietly = TRUE))

fonte <- file.path(
  "projeto_agna", "data", "processed",
  "brasil_convergencia_china_1997_2016.csv"
)
saida <- file.path("listas", "lista_02", "dados", "convergencia_anual_1997_2016.csv")
registro <- file.path("listas", "lista_02", "docente", "validacao", "resultados.txt")

votos <- read.csv(fonte, stringsAsFactors = FALSE)
votos$data <- as.Date(votos$data)
stopifnot(nrow(votos) == 1762L)
stopifnot(!anyDuplicated(votos$rcid))
stopifnot(!anyNA(votos[c("rcid", "data", "ano", "voto_brasil", "voto_china", "convergente")]))
stopifnot(all(votos$ano %in% 1997:2016))
stopifnot(all(as.integer(format(votos$data, "%Y")) == votos$ano))
stopifnot(all(votos$convergente %in% c(0L, 1L)))
stopifnot(all(votos$convergente == as.integer(votos$voto_brasil == votos$voto_china)))

anual <- votos |>
  dplyr::group_by(ano) |>
  dplyr::summarise(
    n_resolucoes_validas = dplyr::n(),
    n_convergentes = sum(convergente),
    taxa_media_convergencia = mean(convergente),
    .groups = "drop"
  ) |>
  dplyr::arrange(ano) |>
  dplyr::select(ano, n_resolucoes_validas, n_convergentes, taxa_media_convergencia)

stopifnot(nrow(anual) == 20L)
stopifnot(identical(anual$ano, 1997:2016))
stopifnot(sum(anual$n_resolucoes_validas) == 1762L)
stopifnot(all(anual$n_resolucoes_validas > 0L))
stopifnot(all(anual$taxa_media_convergencia >= 0 & anual$taxa_media_convergencia <= 1))
stopifnot(isTRUE(all.equal(
  anual$taxa_media_convergencia,
  anual$n_convergentes / anual$n_resolucoes_validas
)))

write.csv(anual, saida, row.names = FALSE, fileEncoding = "UTF-8")
dados <- read.csv(saida)
stopifnot(isTRUE(all.equal(as.data.frame(anual), dados, check.attributes = FALSE)))

dados$anos_desde_1997 <- dados$ano - 1997
dados$anos_desde_2009 <- dados$ano - 2009
modelo_1997 <- lm(taxa_media_convergencia ~ anos_desde_1997, data = dados)
modelo_2009 <- lm(taxa_media_convergencia ~ anos_desde_2009, data = dados)
modelo_ano <- lm(taxa_media_convergencia ~ ano, data = dados)
modelo_sem_intercepto <- lm(taxa_media_convergencia ~ 0 + anos_desde_1997, data = dados)

stopifnot(isTRUE(all.equal(unname(coef(modelo_1997)[2]), unname(coef(modelo_2009)[2]))))
stopifnot(isTRUE(all.equal(unname(coef(modelo_1997)[2]), unname(coef(modelo_ano)[2]))))
stopifnot(isTRUE(all.equal(fitted(modelo_1997), fitted(modelo_2009), check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(fitted(modelo_1997), fitted(modelo_ano), check.attributes = FALSE)))
stopifnot(isTRUE(all.equal(
  unname(coef(modelo_2009)[1]),
  unname(coef(modelo_1997)[1] + 12 * coef(modelo_1997)[2])
)))
stopifnot(abs(mean(resid(modelo_1997))) < 1e-12)
stopifnot(abs(mean(resid(modelo_sem_intercepto))) > 1e-4)
stopifnot(sum(resid(modelo_sem_intercepto)^2) > sum(resid(modelo_1997)^2))

dados$x_z <- as.numeric(scale(dados$anos_desde_1997))
dados$y_z <- as.numeric(scale(dados$taxa_media_convergencia))
modelo_z <- lm(y_z ~ x_z, data = dados)
stopifnot(abs(mean(dados$x_z)) < 1e-12)
stopifnot(abs(sd(dados$x_z) - 1) < 1e-12)
stopifnot(abs(mean(dados$y_z)) < 1e-12)
stopifnot(abs(sd(dados$y_z) - 1) < 1e-12)
stopifnot(isTRUE(all.equal(unname(coef(modelo_z)[2]), unname(cor(dados$x_z, dados$y_z)))))

set.seed(6183)
n <- 40L
X <- rnorm(n, mean = 10, sd = 3)
u <- rnorm(n, mean = 0, sd = 2)
Y <- 5 + 1.5 * X + u
modelo_simulado <- lm(Y ~ X)
stopifnot(abs(mean(resid(modelo_simulado))) < 1e-12)
stopifnot(abs(mean(u)) > 1e-4)
stopifnot(abs(unname(coef(modelo_simulado)[2]) - 1.5) > 1e-4)

inclinacoes <- replicate(100L, {
  x <- rnorm(n, mean = 10, sd = 3)
  erro <- rnorm(n, mean = 0, sd = 2)
  y <- 5 + 1.5 * x + erro
  unname(coef(lm(y ~ x))[2])
})
stopifnot(length(inclinacoes) == 100L)
stopifnot(all(is.finite(inclinacoes)))

linhas <- c(
  "Lista 2 (2026): validação docente",
  paste("Fonte:", fonte),
  paste("Arquivo anual:", saida),
  paste("Resoluções:", nrow(votos)),
  paste("Anos:", nrow(dados)),
  sprintf("MQO 1997: intercepto %.9f; inclinacao %.9f", coef(modelo_1997)[1], coef(modelo_1997)[2]),
  sprintf("MQO 2009: intercepto %.9f; inclinacao %.9f", coef(modelo_2009)[1], coef(modelo_2009)[2]),
  sprintf("Padronizada: intercepto %.9f; inclinacao %.9f", coef(modelo_z)[1], coef(modelo_z)[2]),
  sprintf("Residuo medio com intercepto: %.12f", mean(resid(modelo_1997))),
  sprintf("Residuo medio sem intercepto: %.9f", mean(resid(modelo_sem_intercepto))),
  sprintf("SQR com intercepto: %.9f", sum(resid(modelo_1997)^2)),
  sprintf("SQR sem intercepto: %.9f", sum(resid(modelo_sem_intercepto)^2)),
  sprintf("Simulacao unica: alfa %.9f; beta %.9f; media do erro %.9f", coef(modelo_simulado)[1], coef(modelo_simulado)[2], mean(u)),
  sprintf("100 simulacoes: media beta %.9f; minimo %.9f; maximo %.9f", mean(inclinacoes), min(inclinacoes), max(inclinacoes)),
  "Todas as verificacoes programadas: PASS"
)
writeLines(linhas, registro, useBytes = TRUE)
cat(paste(linhas, collapse = "\n"), "\n")
