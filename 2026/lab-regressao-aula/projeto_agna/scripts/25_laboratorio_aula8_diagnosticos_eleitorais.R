# Aula 8: resíduos e diagnóstico com os dados eleitorais da aula 7
# Abra lab-regressao-aula-2026.Rproj no RStudio e execute por blocos.
# Fonte: TSE. Uma linha é uma zona; percentuais usam todos os votos válidos.

library(dplyr)
library(ggplot2)
library(here)
here::i_am("projeto_agna/scripts/25_laboratorio_aula8_diagnosticos_eleitorais.R")
options(scipen = 999)

# 1. Dados e modelos (10 min) -------------------------------------------

dados_2022 <- read.csv(here("projeto_agna", "data", "eleicoes_2022_2026",
                          "processed", "presidente_2022_zonas_com_exterior.csv"),
                       encoding = "UTF-8")

# O Exterior (ZZ) será examinado no final; começamos com as 27 UFs.
brasil <- dados_2022 |>
  filter(uf != "ZZ") |>
  mutate(uf = factor(uf))

nrow(brasil)
head(dplyr::select(brasil, uf, zona, pct_lula_t1, pct_lula_t2))

modelo_simples <- lm(pct_lula_t2 ~ pct_lula_t1, data = brasil)
modelo_uf <- lm(pct_lula_t2 ~ pct_lula_t1 + uf, data = brasil)

coef(modelo_simples)
coef(modelo_uf)

# Pergunta 1. Interprete a inclinação no modelo com UF. Ela representa
# um efeito causal da votação no primeiro turno sobre a do segundo?

# 2. Resíduos e valores ajustados (20 min) -------------------------------

brasil <- brasil |>
  mutate(ajustado = fitted(modelo_uf), residuo = resid(modelo_uf))

diagnosticos <- bind_rows(
  data.frame(ajustado = fitted(modelo_simples), residuo = resid(modelo_simples),
             modelo = "Somente primeiro turno"),
  data.frame(ajustado = fitted(modelo_uf), residuo = resid(modelo_uf),
             modelo = "Primeiro turno + UF")
)

ggplot(diagnosticos, aes(ajustado, residuo)) +
  geom_point(alpha = 0.2) +
  geom_hline(yintercept = 0, color = "blue") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "darkgreen") +
  facet_wrap(~modelo) +
  labs(title = "Figura 1. Resíduos dos modelos eleitorais de 2022",
       x = "Lula no segundo turno: valor ajustado (%)", y = "Resíduo (p.p.)") +
  theme_minimal()

mean(brasil$residuo)
coef(lm(residuo ~ pct_lula_t1, data = brasil))

# Pergunta 2. Onde aparecem padrões nos resíduos? O que muda ao incluir UF?
# Pergunta 3. Por que a média dos resíduos e a inclinação acima são próximas
# de zero? Isso garante uma CEF linear? Use a curva verde para explicar.

# 3. Magnitude dos resíduos (10 min) ------------------------------------

ggplot(brasil, aes(ajustado, abs(residuo))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "darkgreen") +
  labs(title = "Figura 2. Magnitude dos resíduos do modelo com UF",
       x = "Lula no segundo turno: valor ajustado (%)",
       y = "Valor absoluto do resíduo (p.p.)") +
  theme_minimal()

# Pergunta 4. A dispersão parece constante? O que esse padrão implica para
# a aproximação da média e para os erros-padrão? Considere as duas figuras.

# 4. Alavancagem e influência (20 min) ----------------------------------

brasil$alavancagem <- hatvalues(modelo_uf)
brasil$mudanca_inclinacao <- dfbeta(modelo_uf)[, "pct_lula_t1"]

# A mudança é a inclinação completa menos a inclinação sem aquela zona.
zonas_influentes <- brasil |>
  arrange(desc(abs(mudanca_inclinacao))) |>
  dplyr::select(uf, zona, residuo, alavancagem, mudanca_inclinacao)
head(zonas_influentes)

ggplot(brasil, aes(alavancagem, residuo)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, color = "blue") +
  labs(title = "Figura 3. Alavancagem e resíduos do modelo com UF",
       x = "Alavancagem", y = "Resíduo (p.p.)") +
  theme_minimal()

# Pergunta 5. Um resíduo grande basta para uma zona alterar muito a
# inclinação? E alavancagem alta? Examine o gráfico e a tabela.

zona_escolhida <- slice(zonas_influentes, 1)
sem_zona <- brasil |>
  filter(!(uf == zona_escolhida$uf & zona == zona_escolhida$zona))
modelo_sem_zona <- lm(pct_lula_t2 ~ pct_lula_t1 + uf, data = sem_zona)

c(completa = unname(coef(modelo_uf)["pct_lula_t1"]),
  sem_zona = unname(coef(modelo_sem_zona)["pct_lula_t1"]))

# Pergunta 6. A mudança é substantivamente importante? Que informações
# você buscaria antes de decidir manter ou excluir essa observação?

# 5. A observação do Exterior (5 min) -----------------------------------

modelo_com_exterior <- lm(pct_lula_t2 ~ pct_lula_t1 + uf, data = dados_2022)
dados_2022$residuo <- resid(modelo_com_exterior)
dados_2022$alavancagem <- hatvalues(modelo_com_exterior)

dados_2022 |>
  filter(uf == "ZZ") |>
  dplyr::select(uf, zona, pct_lula_t1, pct_lula_t2, residuo, alavancagem)

# Pergunta 7. Por que o resíduo de ZZ é praticamente zero e sua alavancagem
# é 1? Esse ajuste perfeito indica uma previsão precisa para outra eleição?

# 6. Conclusão (10 min) -------------------------------------------------

# Pergunta 8. Em 5 a 7 linhas, avalie o modelo com UF para prever votação.
# Use os diagnósticos e explique o que ainda precisaria ser avaliado antes
# de aplicá-lo a 2026. Resíduos não normais bastariam para abandonar MQO?
# Entregue os três gráficos e as respostas às perguntas.
