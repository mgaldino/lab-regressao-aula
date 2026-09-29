# Aula 6: entrega e validação

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
