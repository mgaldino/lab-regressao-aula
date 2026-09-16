# Laboratório da Aula 4: resultados potenciais e mecanismos de seleção
#
# Objetivos:
# 1. distinguir resultados potenciais de resultado observado;
# 2. calcular efeito individual, ATE e ATT em uma população fictícia;
# 3. decompor a Diferença Simples de Médias (SDO) em ATT e viés de seleção;
# 4. comparar autoseleção, seleção por terceiros e atribuição aleatória;
# 5. retornar ao AGNA, avaliar se "tema" define uma intervenção e explicar
#    por que a diferença entre períodos permanece descritiva.

options(scipen = 999)
set.seed(20260916)

library(data.table)
library(dplyr)
library(ggplot2)
library(here)

diretorio_saida <- here("projeto_agna", "output", "aula_04")
dir.create(diretorio_saida, recursive = TRUE, showWarnings = FALSE)

# Função auxiliar ---------------------------------------------------------

# `resumir_mecanismo()` calcula as quantidades causais porque, nos exemplos
# simulados, conhecemos os dois resultados potenciais de cada unidade.
resumir_mecanismo <- function(dados, tratamento, resultado) {
  d <- dados[[tratamento]]
  y <- dados[[resultado]]

  ate <- mean(dados$tau)
  att <- mean(dados$tau[d == 1])
  atu <- mean(dados$tau[d == 0])
  sdo <- mean(y[d == 1]) - mean(y[d == 0])
  vies_selecao <- mean(dados$y0[d == 1]) - mean(dados$y0[d == 0])
  proporcao_tratada <- mean(d)
  vies_heterogeneidade <- (1 - proporcao_tratada) * (att - atu)

  data.frame(
    ate = ate,
    att = att,
    atu = atu,
    sdo = sdo,
    vies_selecao = vies_selecao,
    vies_heterogeneidade = vies_heterogeneidade,
    proporcao_tratada = proporcao_tratada
  )
}

# 1. Resultados potenciais: a tabela que a realidade não entrega ----------

# A população fictícia reproduz o exemplo da Médica Perfeita de Cunningham.
# `y1` mede anos adicionais de vida com ventilador; `y0`, sem ventilador.
pacientes <- data.frame(
  id = 1:10,
  y1 = c(7, 5, 5, 7, 4, 10, 1, 5, 3, 9),
  y0 = c(1, 6, 1, 8, 2, 1, 10, 6, 7, 8)
) |>
  dplyr::mutate(tau = y1 - y0)

stopifnot(
  nrow(pacientes) == 10,
  !anyDuplicated(pacientes$id),
  !anyNA(pacientes),
  all(pacientes$tau == pacientes$y1 - pacientes$y0)
)

ate_pacientes <- mean(pacientes$tau)

cat("\nTabela 1. Resultados potenciais conhecidos pelo oráculo\n")
cat(
  "Unidade: paciente. Y(1): anos adicionais com ventilador;",
  "Y(0): anos adicionais sem ventilador.\n"
)
print(pacientes)
cat("\nATE da população fictícia =", round(ate_pacientes, 2), "ano.\n")

# Perguntas para a turma:
# a) Qual é o efeito causal individual para a paciente 1? E para a 2?
# b) Quantas dessas quatro colunas observaríamos em dados reais?
# c) Por que conhecer o ATE nesta tabela depende de uma posição de oráculo?

# 2. Seleção orientada pelos resultados potenciais ------------------------

# A Médica Perfeita atribui o ventilador apenas a quem viveria mais com ele.
# Portanto, D depende diretamente do efeito causal individual.
pacientes <- pacientes |>
  dplyr::mutate(
    d_perfeita = as.integer(tau > 0),
    y_perfeita = d_perfeita * y1 + (1 - d_perfeita) * y0,
    contrafactual_perfeita = d_perfeita * y0 + (1 - d_perfeita) * y1
  )

resumo_perfeita <- resumir_mecanismo(
  pacientes,
  tratamento = "d_perfeita",
  resultado = "y_perfeita"
)

stopifnot(
  isTRUE(all.equal(resumo_perfeita$ate, 0.6)),
  isTRUE(all.equal(resumo_perfeita$att, 4.4)),
  isTRUE(all.equal(resumo_perfeita$sdo, -0.4)),
  isTRUE(all.equal(resumo_perfeita$vies_selecao, -4.8)),
  isTRUE(all.equal(
    resumo_perfeita$sdo,
    resumo_perfeita$att + resumo_perfeita$vies_selecao
  ))
)

cat("\nTabela 2. Atribuição pela Médica Perfeita\n")
cat(
  "D=1 quando Y(1)>Y(0). Y é o resultado observado;",
  "o contrafactual aparece apenas porque os dados são fictícios.\n"
)
print(
  pacientes |>
    dplyr::select(
      id,
      y0,
      y1,
      tau,
      d_perfeita,
      y_perfeita,
      contrafactual_perfeita
    )
)

cat("\nDecomposição da Diferença Simples de Médias (SDO)\n")
cat(
  "SDO =",
  round(resumo_perfeita$sdo, 2),
  "\nATT =", round(resumo_perfeita$att, 2),
  "\nViés de seleção =", round(resumo_perfeita$vies_selecao, 2),
  "\nATT + viés de seleção =",
  round(resumo_perfeita$att + resumo_perfeita$vies_selecao, 2),
  "\n"
)

# Perguntas para a turma:
# a) Por que a associação é negativa se o ATE é positivo?
# b) O problema é a médica tomar decisões ruins ou tomar decisões informadas?
# c) Qual resultado potencial distingue os dois grupos mesmo sem ventilador?

# 3. Atribuição aleatória na mesma população ------------------------------

# Uma realização: cinco das dez pacientes recebem o ventilador por sorteio.
ids_tratados <- sample(pacientes$id, size = 5, replace = FALSE)

pacientes <- pacientes |>
  dplyr::mutate(
    d_aleatorio = as.integer(id %in% ids_tratados),
    y_aleatorio = d_aleatorio * y1 + (1 - d_aleatorio) * y0
  )

resumo_sorteio_realizado <- resumir_mecanismo(
  pacientes,
  tratamento = "d_aleatorio",
  resultado = "y_aleatorio"
)

cat("\nTabela 3. Uma realização da atribuição aleatória completa\n")
print(
  pacientes |>
    dplyr::select(id, y0, y1, tau, d_aleatorio, y_aleatorio)
)
cat(
  "\nNesta realização, SDO =",
  round(resumo_sorteio_realizado$sdo, 2),
  "e ATE =", round(resumo_sorteio_realizado$ate, 2), "ano.\n"
)

# Todas as 252 atribuições possíveis com cinco tratadas e cinco controles.
atribuicoes_possiveis <- combn(pacientes$id, 5)

resultados_atribuicoes <- lapply(
  seq_len(ncol(atribuicoes_possiveis)),
  function(j) {
    d_j <- as.integer(pacientes$id %in% atribuicoes_possiveis[, j])
    y_j <- d_j * pacientes$y1 + (1 - d_j) * pacientes$y0
    dados_j <- pacientes |>
      dplyr::mutate(d_j = d_j, y_j = y_j)

    resumo_j <- resumir_mecanismo(dados_j, "d_j", "y_j")

    data.frame(
      atribuicao = j,
      sdo = resumo_j$sdo,
      att = resumo_j$att,
      atu = resumo_j$atu,
      vies_selecao = resumo_j$vies_selecao,
      vies_heterogeneidade = resumo_j$vies_heterogeneidade
    )
  }
) |>
  dplyr::bind_rows()

media_sorteios <- resultados_atribuicoes |>
  dplyr::summarise(
    sdo = mean(sdo),
    att = mean(att),
    atu = mean(atu),
    vies_selecao = mean(vies_selecao),
    vies_heterogeneidade = mean(vies_heterogeneidade)
  ) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ ifelse(abs(.x) < 1e-12, 0, .x)
    )
  )

stopifnot(
  nrow(resultados_atribuicoes) == choose(10, 5),
  isTRUE(all.equal(media_sorteios$sdo, ate_pacientes)),
  isTRUE(all.equal(media_sorteios$att, ate_pacientes)),
  isTRUE(all.equal(media_sorteios$atu, ate_pacientes)),
  abs(media_sorteios$vies_selecao) < 1e-12,
  abs(media_sorteios$vies_heterogeneidade) < 1e-12
)

cat("\nTabela 4. Média sobre todas as 252 atribuições possíveis\n")
print(media_sorteios)
cat(
  "\nO sorteio não obriga uma realização a coincidir com o ATE.",
  "Sobre todas as atribuições possíveis, a SDO média coincide com o ATE",
  "e os dois componentes de viés têm média zero.\n"
)

figura_aleatorizacao <- ggplot(
  resultados_atribuicoes,
  aes(x = sdo)
) +
  geom_histogram(
    binwidth = 0.5,
    boundary = 0,
    color = "white",
    fill = "#1D4ED8"
  ) +
  geom_vline(
    xintercept = ate_pacientes,
    linewidth = 1.1,
    color = "#B91C1C"
  ) +
  annotate(
    "text",
    x = ate_pacientes + 0.25,
    y = Inf,
    label = "ATE = 0,6",
    hjust = 0,
    vjust = 1.5,
    color = "#B91C1C"
  ) +
  labs(
    title = "SDO sob todas as atribuições aleatórias",
    subtitle = "Cinco pacientes tratados em cada uma das 252 atribuições possíveis",
    x = "Diferença Simples de Médias (SDO)",
    y = "Número de atribuições",
    caption = "A linha vermelha marca o ATE da população fictícia."
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(
    diretorio_saida,
    "distribuicao_diferencas_aleatorias.pdf"
  ),
  plot = figura_aleatorizacao,
  width = 9,
  height = 5.2,
  units = "in"
)

print(figura_aleatorizacao)

# 4. Ciência política: contato de campanha e direcionamento estratégico ----

# Todos os eleitores ganhariam um ponto de apoio ao candidato se contatados.
# A campanha, porém, procura apenas quem já teria apoio alto sem contato.
eleitores <- data.frame(
  id = 1:8,
  y0 = 2:9
) |>
  dplyr::mutate(
    y1 = y0 + 1,
    tau = y1 - y0,
    d_direcionado = as.integer(y0 >= 6),
    y_direcionado = d_direcionado * y1 + (1 - d_direcionado) * y0
  )

resumo_campanha <- resumir_mecanismo(
  eleitores,
  tratamento = "d_direcionado",
  resultado = "y_direcionado"
)

stopifnot(
  isTRUE(all.equal(resumo_campanha$ate, 1)),
  isTRUE(all.equal(resumo_campanha$att, 1)),
  isTRUE(all.equal(resumo_campanha$sdo, 5)),
  isTRUE(all.equal(resumo_campanha$vies_selecao, 4))
)

cat("\nTabela 5. Contato de campanha dirigido a apoiadores prováveis\n")
print(eleitores)
cat(
  "\nSDO =", resumo_campanha$sdo,
  "\nATT =", resumo_campanha$att,
  "\nViés de seleção =", resumo_campanha$vies_selecao,
  "\n"
)

# Exercício:
# a) Explique por que a SDO é cinco vezes o efeito causal.
# b) Troque a regra por um sorteio de quatro eleitores. O que muda?
# c) Faça a campanha procurar os eleitores com menor Y(0). Qual é o sinal do
#    viés de seleção? O efeito causal individual mudou?

# 5. AGNA: categorias temáticas não definem sozinhas uma intervenção -------

# A primeira linha formaliza a pergunta ampla usada na aula anterior. A
# segunda linha mostra como uma pergunta mais estreita pode explicitar o que
# muda e o que permanece fixo. Isso define o estimando, não sua identificação.
diagnostico_intervencao_agna <- data.frame(
  formulacao = c(
    "Efeito do tema ser direitos humanos em vez de desarmamento",
    paste(
      "Efeito de enquadrar a mesma proposta como direitos humanos em vez",
      "de segurança, mantendo fixo o conteúdo substantivo"
    )
  ),
  unidade = c(
    "Resolução da AGNU",
    "Mesma proposta no mesmo contexto decisório"
  ),
  d1 = c(
    "Resolução classificada como direitos humanos",
    "Texto com enquadramento de direitos humanos"
  ),
  d0 = c(
    "Resolução classificada como desarmamento",
    "Texto com enquadramento de segurança"
  ),
  o_que_fica_fixo = c(
    "Não está claro",
    "Conteúdo substantivo, atores, momento e contexto"
  ),
  diagnostico = c(
    paste(
      "Mal definida: trocar o tema pode trocar o problema, o texto, os",
      "atores e a própria unidade"
    ),
    paste(
      "Estimando definido, mas não identificado pelo banco observacional",
      "atual"
    )
  )
)

stopifnot(
  nrow(diagnostico_intervencao_agna) == 2,
  !anyNA(diagnostico_intervencao_agna),
  all(nzchar(unlist(diagnostico_intervencao_agna)))
)

cat("\nTabela 6. Tema, intervenção e estimando no AGNA\n")
cat(
  "Legenda: diagnóstico conceitual. Uma formulação causal precisa dizer",
  "o que muda e o que permanece fixo para a mesma unidade.\n"
)
print(diagnostico_intervencao_agna)

cat(
  "\nEstimando hipotético mais preciso:\n",
  "ATE_enquadramento = média de Y_i(direitos humanos) -",
  "Y_i(segurança), para as mesmas propostas e contextos decisórios.\n",
  "O banco atual não observa versões alternativas da mesma proposta nem",
  "um mecanismo que atribua os enquadramentos. Clareza do estimando não",
  "substitui uma estratégia de identificação.\n"
)

# Exercício de formulação:
# a) Na comparação "direitos humanos versus desarmamento", liste tudo o que
#    muda além do rótulo do tema.
# b) Escolha uma estratégia discutida por Imai (2021): característica
#    percebida, reinterpretação, redefinição ou intervenção no grupo.
# c) Proponha uma intervenção que altere uma única dimensão da resolução.
# d) Defina unidade, D=1, D=0, Y, população-alvo e momento de mensuração.
# e) Escreva Y_i(1), Y_i(0) e o estimando médio correspondente.

# 6. Retorno ao AGNA: SDO sem intervenção definida ----------------------

banco <- fread(
  here(
    "projeto_agna",
    "data",
    "processed",
    "brasil_convergencia_china_1997_2016.csv"
  )
)

dados_agna <- banco |>
  dplyr::select(
    rcid,
    data,
    ano,
    periodo_2009,
    tema,
    voto_brasil,
    voto_china,
    convergente
  )

validacoes_agna <- data.frame(
  verificacao = c(
    "Número esperado de 1.762 linhas",
    "Uma linha por resolução",
    "Sem valores ausentes nas colunas usadas",
    "Ano entre 1997 e 2016",
    "Período coincide com o ano",
    "Convergência assume apenas 0 ou 1",
    "Convergência coincide com a igualdade dos votos"
  ),
  problemas = c(
    abs(nrow(dados_agna) - 1762),
    sum(duplicated(dados_agna$rcid)),
    sum(!complete.cases(dados_agna)),
    sum(!dados_agna$ano %in% 1997:2016, na.rm = TRUE),
    sum(
      dados_agna$periodo_2009 != ifelse(
        dados_agna$ano <= 2008,
        "1997-2008",
        "2009-2016"
      ),
      na.rm = TRUE
    ),
    sum(!dados_agna$convergente %in% c(0, 1), na.rm = TRUE),
    sum(
      dados_agna$convergente != as.integer(
        dados_agna$voto_brasil == dados_agna$voto_china
      ),
      na.rm = TRUE
    )
  )
)

stopifnot(all(validacoes_agna$problemas == 0))

resumo_periodos <- dados_agna |>
  dplyr::group_by(periodo_2009) |>
  dplyr::summarise(
    n_resolucoes = dplyr::n(),
    convergencia_media = mean(convergente),
    .groups = "drop"
  ) |>
  dplyr::arrange(periodo_2009)

diferenca_agna_pp <- 100 * (
  resumo_periodos$convergencia_media[
    resumo_periodos$periodo_2009 == "2009-2016"
  ] -
    resumo_periodos$convergencia_media[
      resumo_periodos$periodo_2009 == "1997-2008"
    ]
)

stopifnot(abs(diferenca_agna_pp - 3.94) < 0.01)

cat("\nTabela 7. Validações da base AGNA\n")
print(validacoes_agna)
cat("\nTabela 8. Convergência Brasil-China por período\n")
print(resumo_periodos)
cat(
  "\nDiferença 2009-2016 menos 1997-2008 =",
  round(diferenca_agna_pp, 2),
  "pontos percentuais.\n"
)

cat(
  "\nDiscussão final:\n",
  "1. O indicador de período separa observações no tempo, mas qual ação ou",
  "intervenção ele representa?\n",
  "2. Que processo colocou certas resoluções em cada período e alterou a",
  "agenda de votações?\n",
  "3. Como definiríamos Y_i(1) e Y_i(0) para a mesma resolução?\n",
  "4. Sem essas respostas, 3,94 pontos percentuais descrevem a amostra;",
  "não identificam um efeito causal.\n"
)

# 7. Arquivos reproduzíveis usados nos slides -----------------------------

resumo_mecanismos <- dplyr::bind_rows(
  dplyr::mutate(resumo_perfeita, mecanismo = "Médica Perfeita"),
  data.frame(
    ate = ate_pacientes,
    att = media_sorteios$att,
    atu = media_sorteios$atu,
    sdo = media_sorteios$sdo,
    vies_selecao = media_sorteios$vies_selecao,
    vies_heterogeneidade = media_sorteios$vies_heterogeneidade,
    proporcao_tratada = 0.5,
    mecanismo = "Média das 252 atribuições aleatórias"
  ),
  dplyr::mutate(resumo_campanha, mecanismo = "Direcionamento de campanha")
) |>
  dplyr::select(
    mecanismo,
    ate,
    att,
    atu,
    sdo,
    vies_selecao,
    vies_heterogeneidade,
    proporcao_tratada
  )

fwrite(
  pacientes,
  file.path(diretorio_saida, "pacientes_resultados_potenciais.csv")
)
fwrite(
  resumo_mecanismos,
  file.path(diretorio_saida, "resumo_mecanismos_selecao.csv")
)
fwrite(
  resultados_atribuicoes,
  file.path(diretorio_saida, "atribuicoes_aleatorias.csv")
)
fwrite(
  resumo_periodos,
  file.path(diretorio_saida, "resumo_agna_periodos.csv")
)
fwrite(
  diagnostico_intervencao_agna,
  file.path(diretorio_saida, "diagnostico_intervencao_agna.csv")
)

cat(
  "\nPASS: resultados potenciais, decomposições, 252 atribuições aleatórias,",
  "duas formulações de intervenção e sete validações AGNA foram executados",
  "sem inconsistências.\n"
)
