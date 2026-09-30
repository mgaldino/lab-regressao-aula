# Aula 6: entrega e validação

> **Versão vigente: seção "Reestruturação de 29 de setembro de 2026" ao final.** As seções intermediárias descrevem a primeira entrega (26 páginas), preservada na tag Git `aula06-v1` (`git show aula06-v1:2026/lab-regressao-aula/projeto_agna/output/pdf/slides_aula_06_regressao_multipla.pdf`).

**Data da aula:** 30 de setembro de 2026. **Escopo:** regressão múltipla, interpretação condicional, preditor categórico e variável omitida. A aula parte de MQO bivariado (Aula 5) e encerra antes de pressupostos, inferência e interações, previstos para aulas posteriores.

## Arquivos

- Fonte editável: projeto_agna/laboratorios/slides_aula_06_regressao_multipla.Rmd.
- Slides compilados: projeto_agna/output/pdf/slides_aula_06_regressao_multipla.pdf (26 páginas, 16:9).
- Laboratório executável: projeto_agna/scripts/09_laboratorio_aula6_regressao_multipla.R.
- Verificação técnica docente: projeto_agna/scripts/10_validar_aula6_regressao_multipla.R.

## Dados, unidade e resultados

Fonte local consultada em **29/09/2026**: projeto_agna/data/processed/painel_pais_ano_1997_2016.csv, derivado das votações nominais da AGNU (unvotes) e de synth_data.rds. Proveniência e dicionário: data/processed/manifesto_dados.csv e data/processed/dicionario_variaveis.csv. SHA-256 do painel lido: 7e1ce8ac8caa37d6d6b6b39604d8fdf7b08275386acc106bf05e322fb50e2b4e, igual ao manifesto.

O painel tem 1.920 pares país–ano únicos (96 países, 1997–2016). A regressão usa somente 2016: 96 países, dos quais 20 latino-americanos, e nenhuma ausência nas variáveis dos modelos. Houve 114 votações no ano; os denominadores da taxa de convergência variam de 5 a 113 pares de votos válidos por país (mediana 112). O script confere tipos, chaves, ausências, limites da taxa e a identidade exata entre taxa, numerador e denominador.

Os quatro modelos usam a **mesma amostra**. O coeficiente do campo de comércio com a China, em pontos percentuais da taxa por um desvio-padrão do preditor, é 0,80 (M1), 0,52 (M2), 1,29 (M3) e 1,69 (M4). O coeficiente de América Latina em M4 é 6,70 pontos percentuais relativo a outras regiões. Com os preditores contínuos no centro da amostra, M4 prevê 60,45% para outras regiões e 67,15% para América Latina. O R² amostral vai de 0,004 em M1 a 0,398 em M4. O exemplo numérico de quatro unidades foi conferido: coeficiente curto de X igual a 5 e coeficiente com Z igual a 2; a diferença é 3 × 1.

Os campos comerciais estão transformados na fonte e sua unidade original não está documentada no dicionário local. Eles foram padronizados **nos 96 países de 2016**, e nenhum coeficiente foi apresentado como variação em pontos percentuais de comércio. Os controles pci_cur e CA_GDP também entram padronizados, sem interpretação de unidade econômica não documentada. O MQO dá peso igual a cada país, apesar dos diferentes denominadores. Os resultados são associações descritivas, sem estimativa de incerteza ou interpretação causal.

## Reprodução e verificação

A partir da pasta 2026/lab-regressao-aula, com o projeto RStudio aberto:

~~~sh
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla projeto_agna/scripts/09_laboratorio_aula6_regressao_multipla.R
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla projeto_agna/scripts/10_validar_aula6_regressao_multipla.R
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla -e 'rmarkdown::render("projeto_agna/laboratorios/slides_aula_06_regressao_multipla.Rmd", output_format = "beamer_presentation", output_file = "slides_aula_06_regressao_multipla.pdf", output_dir = "projeto_agna/output/pdf")'
~~~

**Executado nesta entrega:** parse e execução integral do roteiro discente; execução do script docente com resultado VALIDACAO_AULA6_OK; compilação Beamer com XeLaTeX; verificações numéricas embutidas no Rmd; comparação de todos os coeficientes com a solução matricial de MQO e checagem das equações normais (erro inferior a 10⁻¹⁰). O PDF final tem 26 páginas e foi renderizado para imagens para auditoria visual; as 26 páginas foram examinadas em folhas de contato e os gráficos das páginas 14 e 17, além das fontes na página 26, em resolução alta. A primeira versão tinha duas retas por região na Figura 1; o agrupamento foi corrigido para uma única reta bivariada e o PDF foi recompilado. Texto extraído e imagens finais não mostraram cortes, sobreposições ou rótulo visível “AGNA”. O arquivo interno projeto_agna mantém seu nome técnico.

**Parecer pedagógico e visual:** sequência curta do exemplo de quatro unidades para o modelo aplicado; símbolos definidos antes do uso; pausas de pergunta no exemplo e no laboratório; dados, denominadores e referências visíveis. O ritmo planejado é cerca de 65 minutos de exposição/discussão e 45 minutos de laboratório, com margem para transição.

Avisos do runtime R: dplyr e ggplot2 foram compilados em R 4.4.3, enquanto a sessão usou R 4.4.2. Não houve aviso substantivo de dados, estimação ou LaTeX na compilação final.

## Conferência independente de 29/09/2026

O roteiro discente e o validador docente foram executados novamente a partir da pasta do curso. O validador retornou `VALIDACAO_AULA6_OK`. As figuras do roteiro passam a ser impressas apenas em sessão interativa, para aparecerem no RStudio sem criar `Rplots.pdf` durante a execução em lote. O PDF foi recompilado após essa alteração; manteve 26 páginas, e as Figuras 1 e 2 foram conferidas na versão final. SHA-256 do PDF final: `ea79a1effe4374e068dc3d9da46ad4cd2fc93b55390383e089fec514f694c6eb`. Não há `Rplots.pdf` residual.

## Reestruturação de 29 de setembro de 2026

Parecer que motivou a revisão: `quality_reports/2026-09-29_revisao-aula06-slides.md`. Plano aprovado: `quality_reports/plans/2026-09-29_aula06-reestruturacao.md`. Formato do encontro: 2 h de teoria e 1h30 de laboratório.

### Slides (21 páginas)

1. Roteiro; da Aula 5 à Aula 6 (notação: $\alpha$, $\beta_j$, $\varepsilon$, $\widehat e_i$, til nos candidatos).
2. BLP com dois preditores e equações normais na população e na amostra (plug-in), forma matricial e posto completo.
3. Interpretação condicional e decomposição do coeficiente (Frisch–Waugh–Lovell), com derivação a partir das equações normais e o exemplo de quatro unidades (resíduos 0; −0,5; 0,5; 0; $\widehat\beta_1=2$).
4. Regressão curta e longa: $\gamma_1=\beta_1+\beta_2\delta$, derivação, tabela de sinais e aplicação com as 1.762 resoluções Brasil–China: inclinação do ano de 0,31 p.p./ano na regressão curta e 0,45 com a indicadora de conflito palestino; $\widehat\beta_2=23{,}2$ p.p.; $\widehat\delta=-0{,}0060$ por ano.
5. Preditor categórico com continente (96 países, 2016): médias por grupo, saturação, armadilha das indicadoras, `relevel()` e retas paralelas com exportações para a China.
6. `lm()`, laboratório, síntese e referências (Hansen 2.21–2.25 e 3.18; Shalizi cap. 12 e 14.3; Flores-Macías e Kreps 2013).

Os números são calculados no próprio Rmd, sem `source()` do roteiro dos alunos. Os slides não trazem R², avisos causais, "AGNA" ou `synth_data.rds`.

### Dados novos

`data/processed/covariaveis_pais_ano_1997_2016.csv`, gerado por `scripts/11_extrair_covariaveis_originais.R` a partir do objeto `final_df` do pipeline do projeto RDD Trade. Traz as covariáveis em escala original (exportações para a China e para os EUA em % das exportações, ITPD-E R03; hiato de poder |GPI dos EUA − GPI do país|; PIB per capita; conta corrente) e o continente. A padronização por `arm::rescale()` dessas variáveis reproduz o painel do curso com erro menor que 10⁻⁸. Proveniência em `data/processed/PROVENIENCIA_covariaveis.md`. Os arquivos processados antigos, o dicionário e o `SHA256SUMS` não foram alterados.

### Laboratório (90 minutos)

`scripts/09_laboratorio_aula6_regressao_multipla.R`: base de 2016 (10 min); continente, `relevel()` e armadilha das indicadoras (15); Europa como variável omitida, com a identidade 0,187 = −0,129 + (−17,38)(−0,0182) (20); decomposição do coeficiente em M2 (20); modelos progressivos M1–M4 (25). Coeficiente das exportações para a China, em p.p. de convergência por p.p. de exportações: 0,19 (M1), −0,11 (M2), 0,03 (M3), 0,05 (M4). Sem R², sem `summary()` e sem perguntas causais.

### Amostra

Brasil e 95 países em que a China não foi o principal destino das exportações de bens em nenhum ano de 1997–2016 (grupo de comparação do controle sintético). Os EUA estão na amostra, com hiato de poder zero; o laboratório registra isso e propõe, como exercício opcional, estimar M3 sem os EUA.

### Verificação

- Compilação XeLaTeX sem erro; 21 páginas; todas renderizadas e inspecionadas.
- `scripts/10_validar_aula6_regressao_multipla.R`: `VALIDACAO_AULA6_OK` (identidades de variável omitida e de decomposição, médias por grupo, `relevel()`, `NA` na armadilha, solução matricial e equações normais em M1–M4, reescala das covariáveis e números dos slides). Nenhum `Rplots.pdf` gerado.
- Busca no texto do PDF por causal, identifica, confundidor, associacional, AGNA, synth, R² e desfecho: nenhuma ocorrência.

### Revisão independente e correções

Um revisor independente apontou 17 problemas (8 relevantes e 9 menores). A adjudicação (`quality_reports/adjudication/aula06-slides/4fd0f98258af/`) confirmou 15, classificou 3 como parciais e reservou 1 ao professor (chamar cada votação nominal de "resolução", como no syllabus; há 1.762 votações e 1.468 símbolos de resolução). Correções aplicadas: reta anual ponderada pelo número de votações (a legenda e a figura passam a ter a mesma inclinação, −0,60 p.p. por ano); médias e coeficientes com duas casas; $\widehat\delta=1$ explícito no exemplo; "regressão de X1 em X2" no lugar de "auxiliar" na decomposição; laboratório compara a decomposição com M2; $G$ categorias no lugar de $k$ e cláusula "demais preditores fixos"; condição da identidade amostral; definição de $Y$ e dos vetores; fórmula com três indicadoras omitidas ($0{,}19=-0{,}11+0{,}30$); retas distinguíveis na Figura 2; Goldberger (1991) nas referências.

### Ajustes pedidos pelo professor (29/09/2026, noite)

- Slide "Roteiro" removido.
- "BLP com dois preditores" mostra a derivada em relação a $\widetilde\beta_1$ e explica que $E[\varepsilon]=0$ e $E[X_j\varepsilon]=0$ valem por construção (condições de primeira ordem), sem hipótese sobre $X$ e $\varepsilon$.
- "Equações normais" fica em somatórios e mostra a solução de $\widehat\beta_1$ com dois preditores ($S_{jl}$), como motivação para a notação matricial.
- Bloco novo de notação matricial (9 slides): vetores e matrizes; transposta; produto; somas em forma matricial; SQR; equações normais $\mathbf X'\widehat{\mathbf e}=\mathbf 0$; inversa e $\widehat{\boldsymbol\beta}=(\mathbf X'\mathbf X)^{-1}\mathbf X'\mathbf Y$; exemplo das quatro unidades ($\mathbf X'\mathbf X$, $\mathbf X'\mathbf Y$, inversa, $\widehat{\boldsymbol\beta}=(1,2,3)'$); BLP em forma matricial e plug-in.
- Decomposição do coeficiente: a versão com somatórios fica como ilustração; entram a matriz de resíduos $\mathbf M_Z$ e a derivação matricial $\widehat\beta_1=(\widehat{\mathbf r}'\widehat{\mathbf r})^{-1}\widehat{\mathbf r}'\mathbf Y$, válida para qualquer número de preditores.
- Regressão curta e longa continua em notação escalar.
- Transposta com linha ($\mathbf X'$) em todo o deck.
- Resultado: 31 páginas; compilação sem erro; `VALIDACAO_AULA6_OK`; números do exemplo matricial conferidos por `stopifnot()` no Rmd.
- Depois, a pedido do professor: slide "MQO no R" cortado (30 páginas) e parte 6 no laboratório, "MQO em forma matricial" (10 min): `model.matrix(modelo_m4)` (96 × 9), `t()`, `%*%`, `solve()`, Tabela 4 comparando a fórmula matricial com `lm()` e conferência de $\mathbf X'\widehat{\mathbf e}=\mathbf 0$. Tempos: Europa omitida 15 min e modelos progressivos 20 min, para manter 90 min. `VALIDACAO_AULA6_OK` confere as dimensões, a igualdade com `lm()` e as equações normais.
- Parte 4 do laboratório refeita a pedido do professor: residualização dupla com $X_1$ = exportações para a China e $X_2$ = indicadora de Europa. O resíduo da convergência na indicadora, regredido no resíduo das exportações, recupera o coeficiente da regressão longa da parte 3 (−0,129), com intercepto zero e os mesmos resíduos. O roteiro calcula a correlação parcial (−0,060, contra 0,066 da correlação simples) e confere inclinação = correlação parcial × dp(resíduo de Y)/dp(resíduo de X1). A Figura 1 passa a mostrar os dois resíduos. M2 passou a ser estimado na parte 5. Os slides da decomposição dizem que residualizar também $Y$ dá o mesmo $\widehat\beta_1$ e definem a correlação parcial; a validação docente confere as quatro identidades.
- Parte 4 ganhou dois gráficos: Figura 1, convergência e exportações para a China sem controle (reta simples, +0,19; países europeus em cor própria e um X na média de cada grupo), e Figura 2, os dois resíduos na indicadora de Europa (cada grupo centrado em zero; reta de −0,13, igual ao coeficiente da regressão longa). A pergunta 3 pede a comparação entre as duas figuras.
- Cuidado com residualizar só a variável resposta (ponto de Peter Hull, post no X de 30/04/2026): a decomposição exige residualizar o preditor do eixo horizontal. O laboratório mostra que a regressão do resíduo de Y nas exportações originais dá −0,125, igual a `coef_longo` × `fracao_variancia` (0,972), e não −0,129; a pergunta 3 pergunta por quê. O slide do exemplo de quatro unidades registra que residualizar só Y dá inclinação 1 em vez de 2 (conferido por `stopifnot()` no Rmd); a validação docente confere a identidade.
- Nota de Peter Hull (2018), "On Residualized Outcome Regressions", citada no laboratório (parte 4, com o aviso de que, com vários preditores e só Y residualizado, os coeficientes se misturam e podem trocar de sinal) e nas referências dos slides.
- Parte 7 do laboratório (10 min), a pedido do professor: simulação com três preditores (`set.seed(6183)`, n = 1.000; X1 e X2 independentes, ambos com correlação 0,6 com o controle X3; Y = X1 + 3X2 − 2X3 + erro). Regressão múltipla: β̂1 = 0,97. Erro comum (Y residualizado só em X3, regredido em X1 e X2 originais): −0,41, sinal trocado. Forma correta (X1 e X2 também residualizados em X3): 0,97. O roteiro calcula Ω = (D′D)⁻¹D′D̃ de Hull (2018) e mostra que Ωβ̂ reproduz o coeficiente errado; os termos fora da diagonal (−0,36 e −0,34) misturam o coeficiente de X2 no de X1. Figura 3: Y residualizado em X3 contra X1 original, inclinação −0,45. Figura 4: Y e X1 residualizados em X2 e X3, inclinação 0,97. Tempos: continente 10 min e modelos progressivos 15 min, para manter 90 min. Pergunta 6 nova; validação confere a troca de sinal, a identidade Ωβ̂ e as inclinações das duas figuras.
- Slide "Laboratório" removido: o professor conduz o laboratório direto no script .R. O deck tem 29 páginas.
- Troca de referência no laboratório: `mutate(continente = relevel(continente, ref = "Américas"))` num banco novo (`paises_2016_americas`) e a mesma regressão de novo, no lugar de `relevel()` dentro da fórmula.
- Slide 15 enxugado: "regressão longa" (ainda não introduzida) virou "regressão múltipla" nos slides 15 e 18, e o conteúdo sobre residualizar Y (mesmo β̂1, mesmos resíduos, correlação parcial, aviso de Hull) foi reunido num slide novo, "Resíduo de Y", depois do exemplo das quatro unidades.
- "Matriz de resíduos" refeito a pedido do professor, sem o vetor genérico v: slide "Resíduo de X₁" (regressão de X₁ em 𝐙 em matrizes: coeficientes π̂ = (𝐙′𝐙)⁻¹𝐙′𝐗₁, ajustados 𝐏_Z𝐗₁, resíduo r̂ = 𝐌_Z𝐗₁ = "X₁ limpo de 𝐙", com os números do exemplo) e slide "Matriz de resíduos" (𝐌_Z limpa qualquer variável de 𝐙; três propriedades em palavras, a terceira dita direto para ê). Deck com 31 páginas.
