# Aula 9: erros-padrão e previsão com uma adaptação do modelo PET
# Abra lab-regressao-aula-2026.Rproj no RStudio e execute por blocos.
# Uma linha é uma zona eleitoral em 2022. Fonte: TSE; dados já no curso.

library(here)
library(ggplot2)
library(sandwich)
here::i_am("projeto_agna/scripts/26_laboratorio_aula9_inferencia_pet.R")
options(scipen = 999)

# 1. Dados e pergunta (10 min) -----------------------------------------

dados <- read.csv(here("projeto_agna", "data", "eleicoes_2022_2026",
                      "processed", "presidente_2022_zonas.csv"))
dados$uf <- factor(dados$uf)

# Os percentuais usam TODOS os votos válidos do respectivo turno.
# Brancos e nulos ficam fora; no T1 entram também os outros candidatos.
nrow(dados)
head(dados[c("uf", "zona", "pct_lula_t1", "pct_lula_t2")])
colSums(is.na(dados))

# Pergunta 1. Qual é o desfecho? Por que isso não mede a transferência
# individual de votos entre candidatos?

# 2. Reservar zonas antes de ajustar qualquer modelo (10 min) -----------

# Cerca de 20% de cada UF ficam para avaliação. A semente fixa a divisão.
# É uma avaliação em outras zonas de 2022, não em outra eleição.
# A reserva é retrospectiva: esta base já apareceu nas aulas anteriores.
set.seed(618309)
indices_teste <- integer(0)
for (estado in levels(dados$uf)) {
  indices_uf <- which(dados$uf == estado)
  sorteados <- sample(indices_uf, size = floor(length(indices_uf) / 5))
  indices_teste <- c(indices_teste, sorteados)
}
treino <- dados[-indices_teste, ]
teste <- dados[indices_teste, ]
c(zonas_treino = nrow(treino), zonas_teste = nrow(teste))

# Mesmas especificações para todas as etapas; não escolher pelo teste.
modelo_simples <- lm(pct_lula_t2 ~ pct_lula_t1, data = treino)
modelo_uf <- lm(pct_lula_t2 ~ pct_lula_t1 + uf, data = treino)
coef(modelo_uf)["pct_lula_t1"]

# UF permite interceptos diferentes, mantendo a mesma inclinação.
# Cada zona recebe peso 1. Não há efeitos aleatórios nem interações.
# Pergunta 2. Interprete a inclinação, em pontos percentuais, comparando
# zonas de uma mesma UF. Essa associação é causal?

# 3. Mesmo coeficiente, dois erros-padrão (15 min) -----------------------

coeficiente <- unname(coef(modelo_uf)["pct_lula_t1"])
ep_convencional <- sqrt(vcov(modelo_uf)["pct_lula_t1", "pct_lula_t1"])
ep_hc3 <- sqrt(vcovHC(modelo_uf, type = "HC3")["pct_lula_t1", "pct_lula_t1"])

erros_padrao <- data.frame(
  metodo = c("Convencional", "HC3"),
  coeficiente = coeficiente,
  erro_padrao = c(ep_convencional, ep_hc3)
)
print(erros_padrao, row.names = FALSE)

# HC3 permite heterocedasticidade; não resolve dependência entre zonas.
# Pergunta 3. O coeficiente mudou? O erro-padrão robusto é sempre maior?

# 4. Bootstrap de pares dentro de cada UF (20 min) ----------------------

# Sortear a linha inteira preserva a relação entre resposta e preditores.
# Mantemos o número de zonas de cada UF em todas as reamostras.
# Supomos independência entre zonas para este exercício de inferência.
set.seed(618310)
repeticoes <- 499
inclinacoes_bootstrap <- numeric(repeticoes)

for (b in seq_len(repeticoes)) {
  indices_bootstrap <- integer(0)
  for (estado in levels(treino$uf)) {
    indices_uf <- which(treino$uf == estado)
    sorteados <- sample(indices_uf, size = length(indices_uf), replace = TRUE)
    indices_bootstrap <- c(indices_bootstrap, sorteados)
  }
  reamostra <- treino[indices_bootstrap, ]
  modelo_bootstrap <- lm(pct_lula_t2 ~ pct_lula_t1 + uf, data = reamostra)
  inclinacoes_bootstrap[b] <- coef(modelo_bootstrap)["pct_lula_t1"]
}

ep_bootstrap <- sd(inclinacoes_bootstrap)
erros_padrao <- rbind(erros_padrao, data.frame(
  metodo = "Bootstrap dentro de UF", coeficiente = coeficiente,
  erro_padrao = ep_bootstrap
))
print(erros_padrao, row.names = FALSE)

figura_bootstrap <- ggplot(data.frame(inclinacao = inclinacoes_bootstrap),
                          aes(inclinacao)) +
  geom_histogram(bins = 25, fill = "#1D4ED8", color = "white") +
  geom_vline(xintercept = coeficiente, color = "#047857", linewidth = 1) +
  labs(x = "Inclinação reestimada (p.p. por p.p.)", y = "Reamostras",
       caption = "Figura 1. Bootstrap de pares dentro de UF, só no treino de 2022.\nLinha verde: inclinação estimada no treino original. Fonte: TSE; adaptação didática.") +
  theme_minimal(base_size = 13)
if (interactive()) print(figura_bootstrap)

# Pergunta 4. Por que usamos sd() das inclinações, sem dividir por sqrt(499)?
# Aumentar o número de reamostras aumenta a informação dos dados originais?
# Este bootstrap não sorteia UFs inteiras: UF é estrato, não cluster.

# 5. Avaliação preditiva nas zonas reservadas (10 min) ------------------

teste$previsao_simples <- predict(modelo_simples, newdata = teste)
teste$previsao_uf <- predict(modelo_uf, newdata = teste)

# RMSE dá mais peso a erros grandes; MAE é a média do erro absoluto.
avaliacao <- data.frame(
  modelo = c("Somente T1", "T1 + UF"),
  rmse_pp = c(sqrt(mean((teste$pct_lula_t2 - teste$previsao_simples)^2)),
              sqrt(mean((teste$pct_lula_t2 - teste$previsao_uf)^2))),
  mae_pp = c(mean(abs(teste$pct_lula_t2 - teste$previsao_simples)),
             mean(abs(teste$pct_lula_t2 - teste$previsao_uf)))
)
print(avaliacao, row.names = FALSE)

# Não truncamos previsões para esconder problemas do modelo linear.
range(teste$previsao_uf)
sum(teste$previsao_uf < 0 | teste$previsao_uf > 100)

figura_previsao <- ggplot(teste, aes(previsao_uf, pct_lula_t2)) +
  geom_point(alpha = 0.35, color = "#334155") +
  geom_abline(intercept = 0, slope = 1, color = "#047857", linewidth = 1) +
  coord_equal() +
  labs(x = "Votação prevista no T2 (%)", y = "Votação observada no T2 (%)",
       caption = "Figura 2. Zonas de teste de 2022, sem uso no ajuste.\nPercentuais sobre todos os votos válidos. Fonte: TSE; adaptação didática.") +
  theme_minimal(base_size = 13)
if (interactive()) print(figura_previsao)

# Pergunta 5. O erro-padrão da inclinação mede o erro de previsão de uma zona?
# Avaliar em outras zonas de 2022 garante desempenho em 2026? Por quê?

# 6. Salvar a entrega e concluir (10 min) -------------------------------

saida <- here("projeto_agna", "output", "aula_09")
dir.create(saida, recursive = TRUE, showWarnings = FALSE)
write.csv(erros_padrao, file.path(saida, "erros_padrao.csv"), row.names = FALSE)
write.csv(avaliacao, file.path(saida, "avaliacao_teste_2022.csv"), row.names = FALSE)
ggsave(file.path(saida, "01_bootstrap.pdf"), figura_bootstrap, width = 8, height = 4)
ggsave(file.path(saida, "02_previsao_teste.pdf"), figura_previsao, width = 6, height = 4)
saveRDS(list(erros_padrao = erros_padrao, avaliacao = avaliacao,
             bootstrap = inclinacoes_bootstrap, coeficiente = coeficiente,
             treino = treino, teste = teste), file.path(saida, "resultados_aula_09.rds"))

# Os dados cobrem as zonas de 2022; não são amostra aleatória de eleitores.
# Os EPs descrevem uma repetição hipotética sob as hipóteses do exercício,
# não uma margem de erro da contagem oficial. UF não elimina dependência.
# Não usamos T2 como preditor, nem dados de 2026 ou totais nacionais previstos.
# Entregue as duas figuras, a tabela de EPs e as respostas às 5 perguntas.
# Conclua em 5 linhas: associação, incerteza, previsão e suas limitações.
