# Apoio à proposta de roteiro da Aula 11. Não gera slides nem figuras.
# Entradas observadas de 2022 preservadas nas aulas anteriores.
library(here)
here::i_am("projeto_agna/roteiros/aula_11/2026-11-09/verificar_exemplo.R")
pasta <- here("projeto_agna", "roteiros", "aula_11", "2026-11-09")
anterior <- readRDS(here("projeto_agna", "output", "aula_09", "resultados_aula_09.rds"))
original <- read.csv(here("projeto_agna", "data", "eleicoes_2022_2026", "processed",
                          "presidente_2022_zonas.csv"))
dados <- droplevels(subset(anterior$treino, uf %in% c("RJ", "SP")))
dados$sp <- as.integer(dados$uf == "SP")
dados$x_c <- dados$pct_lula_t1 - 40
campos <- c("uf", "zona", "pct_lula_t1", "pct_lula_t2", "lula_t1", "lula_t2",
            "validos_t1", "validos_t2")
stopifnot(nrow(dados) == 447L, sum(dados$sp == 0) == 132L, sum(dados$sp == 1) == 315L,
          !anyNA(dados[campos]), anyDuplicated(dados[c("uf", "zona")]) == 0L,
          all(vapply(dados[setdiff(campos, "uf")], is.numeric, logical(1))),
          all(dados$validos_t1 > 0), all(dados$validos_t2 > 0),
          all(dados$pct_lula_t1 >= 0 & dados$pct_lula_t1 <= 100),
          all(dados$pct_lula_t2 >= 0 & dados$pct_lula_t2 <= 100))
igual <- function(x, y, tolerancia = 1e-9) {
  stopifnot(length(x) == length(y), all(is.finite(x)), all(is.finite(y)),
            max(abs(x - y)) < tolerancia)
}
chaves <- paste(dados$uf, dados$zona, sep = ":")
pos <- match(chaves, paste(original$uf, original$zona, sep = ":"))
stopifnot(!anyNA(pos))
igual(dados$pct_lula_t1, original$pct_lula_t1[pos])
igual(dados$pct_lula_t2, original$pct_lula_t2[pos])
igual(dados$pct_lula_t1, 100 * dados$lula_t1 / dados$validos_t1)
igual(dados$pct_lula_t2, 100 * dados$lula_t2 / dados$validos_t2)

# Comparações aditivo/interativo usam as MESMAS 447 zonas.
aditivo <- lm(pct_lula_t2 ~ x_c + sp, data = dados)
bruto <- lm(pct_lula_t2 ~ pct_lula_t1 * sp, data = dados)
centrado <- lm(pct_lula_t2 ~ x_c * sp, data = dados)
C <- sandwich::vcovHC(centrado, type = "HC3")
b <- coef(centrado)
nu <- df.residual(centrado)
critico <- qt(.975, df = nu)
stopifnot(nu == 443L, length(b) == 4L)

# Transformação explícita entre coeficientes: centralizar em 40 p.p.
J <- diag(4)
J[1, 2] <- 40
J[3, 4] <- 40
igual(unname(b), drop(J %*% coef(bruto)))
igual(as.numeric(C), as.numeric(J %*% sandwich::vcovHC(bruto, type = "HC3") %*% t(J)))
igual(fitted(centrado), fitted(bruto))
igual(resid(centrado), resid(bruto))

# Duas regressões separadas devem reproduzir as retas do modelo totalmente interativo.
for (grupo in c(0, 1)) {
  linhas <- dados$sp == grupo
  separado <- lm(pct_lula_t2 ~ x_c, data = dados[linhas, ])
  igual(fitted(centrado)[linhas], fitted(separado))
}

resumir_combinacao <- function(L, nome, x = NA_real_) {
  estimativa <- sum(L * b)
  ep <- sqrt(drop(t(L) %*% C %*% L))
  data.frame(quantidade = nome, x_t1 = x, estimativa = estimativa, ep_hc3 = ep,
             inferior = estimativa - critico * ep, superior = estimativa + critico * ep,
             t = estimativa / ep, valor_p = 2 * pt(-abs(estimativa / ep), df = nu))
}

resultados <- rbind(
  resumir_combinacao(c(0, 1, 0, 0), "inclinacao_RJ"),
  resumir_combinacao(c(0, 1, 0, 1), "inclinacao_SP"),
  resumir_combinacao(c(0, 0, 0, 1), "diferenca_inclinacoes_SP_RJ"),
  resumir_combinacao(c(0, 0, 1, 0), "diferenca_niveis_SP_RJ", 40),
  resumir_combinacao(c(0, 0, 1, 10), "diferenca_niveis_SP_RJ", 50)
)
for (x in c(40, 50)) {
  novos <- data.frame(x_c = x - 40, sp = c(0, 1))
  previsoes <- predict(centrado, newdata = novos)
  matriz <- model.matrix(~x_c * sp, data = novos)
  L <- matriz[2, ] - matriz[1, ]
  linha <- subset(resultados, quantidade == "diferenca_niveis_SP_RJ" & x_t1 == x)
  igual(previsoes[2] - previsoes[1], linha$estimativa)
  igual(sqrt(drop(t(L) %*% C %*% L)), linha$ep_hc3)
  dados_novo_centro <- dados
  dados_novo_centro$novo_x <- dados$pct_lula_t1 - x
  outro <- lm(pct_lula_t2 ~ novo_x * sp, data = dados_novo_centro)
  igual(coef(outro)["sp"], linha$estimativa)
  igual(sqrt(sandwich::vcovHC(outro, type = "HC3")["sp", "sp"]), linha$ep_hc3)
}

suporte <- do.call(rbind, lapply(c("RJ", "SP"), function(estado) {
  x <- dados$pct_lula_t1[dados$uf == estado]
  data.frame(uf = estado, n = length(x), minimo = min(x), q10 = unname(quantile(x, .1)),
             mediana = median(x), q90 = unname(quantile(x, .9)), maximo = max(x))
}))
comum <- c(max(suporte$minimo), min(suporte$maximo))
stopifnot(all(c(40, 50) > comum[1]), all(c(40, 50) < comum[2]))

# Exemplo HIPOTÉTICO de duas intervenções binárias: comparecimento em porcentagem.
# E[Y(t,v)]: células construídas pelo professor, não resultados de uma pesquisa.
medias_hipoteticas <- data.frame(t = c(0, 1, 0, 1), v = c(0, 0, 1, 1),
                                comparecimento_pct = c(50, 52, 55, 60))
fatorial <- lm(comparecimento_pct ~ t * v, data = medias_hipoteticas)
igual(coef(fatorial)["t:v"], (60 - 55) - (52 - 50))

numeros <- list(
  data_preparacao = "2026-11-09", ano_dados_observados = 2022L,
  origem = "Subconjunto RJ-SP do treino preservado da Aula 9, sem novas zonas de teste",
  n = nrow(dados), n_RJ = 132L, n_SP = 315L, p = length(b), nu = nu,
  centro = 40, referencia = "RJ (sp=0)", peso = "1 por zona",
  unidade_x_y = "Porcentagem de todos os votos válidos do respectivo turno",
  coeficientes_brutos = as.list(coef(bruto)), coeficientes_centrados = as.list(b),
  coeficientes_aditivo_mesma_amostra = as.list(coef(aditivo)),
  covariancia_HC3_centrada = C, resultados = resultados, suporte = suporte,
  limites_suporte_comum = comum, critico_t = critico,
  inferencia = "HC3 com referência t_443 aproximada e ICs pontuais; sem ajuste para dependência entre zonas",
  exemplo_hipotetico = medias_hipoteticas, interacao_hipotetica_pp = 3,
  verificacoes = c("Dados comparados ao CSV preservado", "Denominadores conferidos",
    "Centralização preserva ajustes e resíduos", "Transformação matricial da covariância confere",
    "Retas conferidas com regressões separadas", "Diferenças e EPs conferidos por mudança de centro",
    "Pontos 40 e 50 no intervalo de suporte comum", "Exemplo hipotético fatorial confere")
)
jsonlite::write_json(numeros, file.path(pasta, "numeros_verificados_v1.json"),
                    pretty = TRUE, auto_unbox = TRUE, digits = 15, na = "null")
write.csv(resultados, file.path(pasta, "resultados_exemplo_v1.csv"), row.names = FALSE)
write.csv(suporte, file.path(pasta, "suporte_exemplo_v1.csv"), row.names = FALSE)
saveRDS(list(dados = dados, bruto = bruto, centrado = centrado, aditivo = aditivo,
             covariancia_hc3 = C, numeros = numeros), file.path(pasta, "exemplo_verificado_v1.rds"))
capture.output(sessionInfo(), file = file.path(pasta, "sessao_R.txt"))
print(resultados, row.names = FALSE, digits = 8)
cat("AULA11_EXEMPLO_VERIFICADO\n")
