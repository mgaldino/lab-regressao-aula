# Aula 7: entrega e reprodução

Preparação: 13/10/2026. Data da aula: 14/10/2026.

## Arquivos

- Fonte dos slides: `projeto_agna/laboratorios/slides_aula_07_propriedades_mqo.Rmd`.
- PDF: `projeto_agna/output/pdf/slides_aula_07_propriedades_mqo.pdf`, 26 páginas, 16:9.
- Cálculos e figuras: `projeto_agna/scripts/12_simulacoes_aula7_propriedades_mqo.R`.
- Verificação analítica: `projeto_agna/scripts/13_validar_aula7_propriedades_mqo.R`.
- Resultados, amostras, estimativas e figuras: nesta pasta. O Rmd lê os resultados e importa as figuras. Os cálculos permanecem no script R.

## Conteúdo e notação

Sequência: BLP e estimador de MQO; duas amostras da mesma população; distribuição amostral; viés; variância e EQM; consistência; cálculo do MQO na amostra e unicidade dos coeficientes; reescrita da fórmula conhecida por divisão dos dois produtos por n; produtos matriciais como somas e médias, com ligação à fórmula escalar; demonstração da consistência e alvo populacional; média condicional zero e exemplo político de gasto de campanha; demonstração do não viés; variância condicional sob variância constante; versão clássica de Gauss–Markov; segunda prova do não viés do MQO por $\mathbf A_0\mathbf X=\mathbf I$; estimador alternativo e comparação de variâncias; Monte Carlo com diferentes tamanhos de amostra; alvo populacional da projeção; bootstrap de pares; síntese e leituras.

Os slides 6–8 esclarecem que o viés é definido pela esperança na distribuição amostral do estimador. A expressão “não viés para n fixo” significa que essa esperança coincide com o parâmetro sem tomar um limite assintótico. O erro de uma estimativa observada é uma realização da diferença entre estimador e parâmetro. A consistência descreve uma sequência quando n cresce.

Notação preservada: `alpha`, `beta_j` e o vetor de coeficientes contendo o intercepto; `epsilon` como erro da projeção linear; `e_hat` como resíduo amostral; transposta com linha. O erro `u` do exemplo causal no slide 14 e da regra geradora no slide 21 é explicitamente distinto do erro da projeção linear. O MQO converge para o coeficiente do BLP, 2,3, no segundo cenário, enquanto a regra original inclui o parâmetro 1,5.

O tema Madrid, as cores, a tipografia, os blocos e o rodapé foram copiados do YAML vigente da Aula 6. A revisão didática de 14/10 dividiu a passagem sobre consistência em três slides. A revisão seguinte retirou o slide de Monte Carlo sobre heterocedasticidade e simplificou o slide de variância. O novo slide 14 adapta o exemplo de independência na média de Cunningham para gastos de campanha e distingue a hipótese substantiva $E[u\mid G]=E[u]$ da normalização $E[u]=0$. O slide 18 começa pelo MQO, deriva $E[\widehat{\boldsymbol\beta}\mid\mathbf X]=\mathbf A_0\mathbf X\boldsymbol\beta$ e verifica $\mathbf A_0\mathbf X=\mathbf I$; o slide 19 introduz o estimador alternativo antes da comparação de variâncias. O PDF tem 26 páginas. O laboratório posterior está documentado na seção “Laboratório de 75 minutos”, ao final.

## Simulações

Dados inteiramente sintéticos. Processo principal: X normal com média 10 e desvio-padrão 3, Y = 5 + 1,5 X + erro, com erro normal independente e desvio-padrão 2. São 5.000 amostras independentes por cenário e 5.000 reamostras bootstrap da primeira amostra observada. As sementes são 618307–618311 e 618315, documentadas nos scripts.

Os cenários incluem n = 10, 40, 160 e 640; aumento do desvio-padrão do erro para 6; heterocedasticidade com variância condicional `2 * (1 + ((X - 10)/3)^2)` e variância incondicional igual a 4; regra geradora com `u = 0,8 * (X - 10) + v`, cujo BLP tem intercepto -3 e inclinação 2,3. A normalidade faz parte desses exemplos e não é uma condição necessária para a versão clássica de Gauss–Markov apresentada.

As médias e dispersões de Monte Carlo são aproximações. A variância amostral populacional e o viés exato são propriedades teóricas sob as hipóteses indicadas. O erro-padrão bootstrap também é uma estimativa e varia conforme a amostra observada.

## Fontes

Programa vigente, entrada da Aula 7 em `syllabus.Rmd`. Aronow e Miller (2019), *Foundations of Agnostic Statistics*, seção 3.2.2, pp. 102–105; complemento para bootstrap, seção 3.4.3, pp. 130–132. Hansen, *Econometrics*, seções 4.4–4.8 e 7.2, com a seção 4.7 sustentando a ressalva sobre existência dos momentos do estimador. Conteúdo conferido nas cópias locais dos livros em Downloads em 13/10/2026. A apresentação usa a versão clássica do teorema de Gauss–Markov, restrita a estimadores lineares e não viesados, e a identifica expressamente.

O slide 14 adapta a lógica do exemplo de escolaridade e salários de Cunningham, [*Causal Inference: The Mixtape*, seção 2.12](https://mixtape-1ed.netlify.app/02-probability_and_regression#eq-exog1), consultada em 14/10/2026. Os níveis de gasto são hipotéticos e não representam dados eleitorais observados.

## Reprodução

Executar a partir de `2026/lab-regressao-aula`:

```sh
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla projeto_agna/scripts/12_simulacoes_aula7_propriedades_mqo.R
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla projeto_agna/scripts/13_validar_aula7_propriedades_mqo.R
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla -e 'rmarkdown::render("projeto_agna/laboratorios/slides_aula_07_propriedades_mqo.Rmd", output_format = "beamer_presentation", output_file = "slides_aula_07_propriedades_mqo.pdf", output_dir = "projeto_agna/output/pdf", output_options = list(keep_tex = TRUE), clean = TRUE)'
```

Dependências: R, ggplot2, dplyr, rmarkdown, knitr, here, Pandoc e XeLaTeX. As figuras vetoriais usam o dispositivo PDF do R, sem depender de Cairo/X11.

## Verificações executadas

O validador retornou `VALIDACAO_AULA7_OK`. Conferiu tipos, ausências, duplicidades e número de repetições; a fórmula escalar contra `lm()`; equações normais; decomposição empírica EQM = variância + viés ao quadrado; médias e variâncias de Monte Carlo contra os alvos e a variância exata do MQO no caso normal homocedástico; esperança e covariância condicionais com X fixa; reamostragem dos pares; estimativas bootstrap selecionadas contra `lm()`; e a diferença positiva semidefinida de covariâncias da prova de Gauss–Markov. Os testes de Monte Carlo usam tolerâncias proporcionais ao erro de simulação.

Na entrega inicial, a compilação XeLaTeX foi concluída. Todas as 22 páginas daquele PDF foram renderizadas e examinadas. As páginas 3, 5, 7, 14, 17 e 20 também foram inspecionadas em resolução maior. A extração de coordenadas do texto encontrou zero palavras fora das páginas. As legendas duplicadas das Figuras 1 e 5 e a ordem dos painéis da Figura 6 foram corrigidas antes da compilação final. A revisão pedagógica conferiu a sequência, as definições de símbolos e as transições entre propriedades do estimador, hipóteses e demonstrações. Não houve teste empírico com dados observacionais.

SHA-256 do PDF da entrega inicial: `d55ee4d9c22fa3615925b6afccb68f6554e226a828517ca269b46d4147a310cb`.

## Laboratório de 75 minutos

Preparado em 13/10/2026, após o pedido de um script discente. Arquivo: `projeto_agna/scripts/14_laboratorio_aula7_monte_carlo_bootstrap.R`. Conferência docente: `projeto_agna/scripts/15_validar_laboratorio_aula7.R`.

| Parte | Tempo previsto | Atividade |
|---|---:|---|
| 1 | 10 min | Gerar uma amostra de 40 pares, ajustar MQO e distinguir erro gerado, resíduo e erro de estimação |
| 2 | 20 min | Gerar 1.000 novas amostras por n (10 e 40), examinar a distribuição das inclinações e aproximar viés, variância, EQM e erro-padrão |
| 3 | 15 min | Comparar n = 40, 160 e 640 e aumentar o desvio-padrão do erro de 2 para 6 |
| 4 | 20 min | Reamostrar 40 pares com reposição 1.000 vezes e estimar o erro-padrão bootstrap |
| 5 | 10 min | Comparar Monte Carlo e bootstrap, escrever uma interpretação e organizar os resultados |

O tempo é uma previsão de atividade em duplas ou trios, incluindo discussão e registro. O script está completo para execução por blocos e contém nove perguntas. A primeira amostra é exatamente a mesma dos slides. O laboratório gera suas próprias amostras e dispensa os resultados previamente produzidos para o deck. Mantém X e Y juntos no bootstrap e esclarece a diferença entre tamanho da amostra, número de repetições de Monte Carlo e número de reamostras bootstrap. A interpretação do viés usa a distribuição amostral. A média de um conjunto finito de estimativas apenas aproxima a esperança do estimador.

Resultados em `projeto_agna/output/aula_07/laboratorio/`: quatro tabelas CSV; dados e estimativas em RDS; cinco objetos ggplot em RDS; Figuras 2b, 3 e 4 em PDF; resultados da validação em CSV e TXT. As figuras aparecem no RStudio ao executar por blocos em uma sessão interativa; a execução em lote exporta sem criar `Rplots.pdf`.

Reprodução a partir da pasta do curso:

```sh
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla projeto_agna/scripts/14_laboratorio_aula7_monte_carlo_bootstrap.R
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla projeto_agna/scripts/15_validar_laboratorio_aula7.R
```

Executado integralmente em uma sessão nova do R. O validador retornou `VALIDACAO_LAB_AULA7_OK`: conferiu a amostra inicial contra os slides, tipos, ausências e chaves; a fórmula matricial e as equações normais; médias e variâncias de Monte Carlo contra as propriedades exatas do caso normal; a decomposição do EQM; todos os 1.000 coeficientes bootstrap contra a solução matricial calculada nos pares selecionados; e os arquivos exportados. As quatro figuras iniciais foram renderizadas e inspecionadas. A Tabela 2 usa impressão com largura completa para exibir o erro-padrão e o EQM no console. A duração da atividade ainda não foi observada em sala.


## Comparação com amostras de tamanho 10

Acrescentada em 13/10/2026 a pedido do professor. O slide 4 e seu histograma original de n = 40 foram mantidos. Um novo slide 5 compara n = 10 e n = 40 com os mesmos eixos e a mesma largura de classe. As linhas identificam o parâmetro populacional 1,5 e a média das estimativas. O deck passa a ter 23 páginas.

Nas 5.000 repetições para n = 10, a média da inclinação foi 1,502989 e o desvio-padrão foi 0,254065. Para n = 40, a média foi 1,501866 e o desvio-padrão foi 0,107372. O novo texto destaca a maior dispersão das estimativas individuais, com a média das repetições próxima do parâmetro. A aproximação numérica não substitui a demonstração teórica de não viés.

O laboratório inclui a Figura 2b e a Tabela 1b, com 1.000 amostras por tamanho n, dentro da parte 2 de 20 minutos. Sua estimativa da média para n = 10 é 1,496551, com desvio-padrão 0,252159. O script segue planejado para 75 minutos. Os resultados de Monte Carlo já existentes foram preservados mediante uma semente própria para o cenário novo.

Reexecutados os scripts 12 e 14 e os validadores 13 e 15. Ambos os validadores passaram, inclusive a comparação da variância simulada em n = 10 com a variância teórica exata. Os sete cenários anteriores foram comparados ao resumo anterior salvo no Git e permaneceram iguais; o PNG do histograma original de n = 40 também permaneceu idêntico. As páginas 4 e 5 do PDF atualizado e a nova figura do laboratório foram renderizadas e inspecionadas. O ajuste final encurtou a frase de interpretação e reduziu ligeiramente a largura da figura para assegurar que todo o conteúdo do slide 5 ficasse visível.

SHA-256 do PDF atualizado em 13/10/2026: `7f4f7d4a31c249347b050136040ce85cbe02cecc7d02d9ebf390b13212d47944`.

## Laboratório eleitoral por zona, versão principal

Preparado em 13/10/2026 após a escolha do professor por resultados eleitorais, unidade de zona eleitoral e porcentagem de votos válidos. O script principal passa a ser `projeto_agna/scripts/18_laboratorio_aula7_previsao_eleitoral.R`. O script 14 de Monte Carlo continua disponível como complemento, com duração própria, fora dos 75 minutos da atividade eleitoral. Os slides não foram alterados por esta aplicação.

| Parte | Tempo previsto | Atividade |
|---|---:|---|
| 1 | 10 min | Unidade UF + zona e denominadores dos percentuais nos dois turnos |
| 2 | 20 min | Porcentagem de Lula em 2022: regressão simples e regressão com UF |
| 3 | 20 min | Aplicação a 2026, previsão do adversário por complemento e agregação ponderada |
| 4 | 15 min | 500 reamostras de pares dentro de UF, mantendo entradas de 2026 fixas |
| 5 | 10 min | Interpretação, hipóteses de aplicação e entrega das respostas |

Treino: 2.636 zonas nas 27 UFs brasileiras, com Lula e Jair Bolsonaro nos dois turnos de 2022. Aplicação: 2.640 zonas, com Lula e Flávio Bolsonaro no primeiro turno de 2026. Cada zona soma votos de todas as seções e municípios abrangidos por ela. A porcentagem é calculada sobre todos os votos válidos do turno. Exterior (ZZ) é excluído. Os dois CSVs de entrada somam aproximadamente 555 KiB.

O ajuste é `pct_lula_t2 ~ pct_lula_t1 + uf`, com peso igual para cada zona. A previsão de Flávio Bolsonaro é 100 menos a previsão de Lula, pois há dois candidatos no segundo turno. A agregação por UF e nas 27 UFs usa votos válidos do primeiro turno de 2026 como pesos. Ela depende da estabilidade da distribuição relativa desses votos entre as zonas. Transportar os coeficientes de 2022 para 2026 depende de outra hipótese: estabilidade da relação entre os turnos. O exercício usa 2022 para estimar e 2026 para aplicar, sem exigir um painel de territórios fixos.

Fontes públicas do TSE, metadados e JSONs oficiais foram consultados em 13/10/2026. Os Parquets por seção, preparados em 05/10/2026 no projeto local electoralFraud, foram preservados por cópia. O snapshot de BUs de 2026 tinha 15 seções ausentes no Brasil, com 3.935 votos válidos. As listas oficiais identificaram todas as ausências de cada município em uma única zona: Betim, MG 319, e Carapicuíba, SP 388. Diferenças com os totais municipais oficiais completaram as somas dessas zonas sem rateio entre zonas nem imputação por seção. Os 12 totais de candidatos e os votos válidos, antes da exclusão de ZZ, fecharam com o JSON nacional oficial. A transformação está registrada no dicionário, no relatório de coleta e em uma tabela de complementos.

Resultado didático do modelo com UF, condicionado aos pesos de 2026 T1: Lula 47,295511% e Flávio Bolsonaro 52,704489% nas 27 UFs. O modelo simples resulta em Lula 47,101481%. Nenhuma previsão de zona ficou fora de 0 a 100. Há 93 zonas de aplicação fora da faixa de porcentagens observada em sua UF no treino. São pontos de discussão no script. O desvio-padrão bootstrap da previsão agregada é 0,030194 p.p. Esse número mede sensibilidade à reamostragem das zonas de treino, condicionando em UF e nos dados de aplicação. Não inclui mudanças entre eleições, dependência entre zonas nem a variabilidade do resultado futuro, portanto não é toda a incerteza da previsão.

Os scripts 17 e 18 executaram integralmente em sessões novas do R. O validador 19 retornou `VALIDACAO_LAB_ELEITORAL_OK`: conferiu chaves, ausências, denominadores, somas de seções e complementos, equações normais, agregação ponderada e todos os 500 sorteios. Calculou coeficientes e previsões por centralização de X e Y dentro de UF, sem usar `lm()` ou `predict()`. A maior diferença entre as previsões bootstrap foi 6,4 × 10^-13 p.p. Os dois gráficos PDF foram renderizados e examinados, com títulos, eixos e captions legíveis.

O script Python 20 produz manifestos e o ZIP discente, com os CSVs, scripts 18 e 14, projeto RStudio e documentação. Verificou a integridade do ZIP e a igualdade dos arquivos empacotados aos originais. O script 18 também foi executado a partir do pacote extraído em uma pasta temporária, e todos os CSVs de resultados permaneceram idênticos aos da execução no curso. Os hashes dos snapshots foram comparados às cópias de origem, e os JSONs brutos foram conferidos contra seus recibos. O pacote ocupa cerca de 282 KiB. A duração prevista ainda não foi observada em sala. A validade preditiva entre eleições não foi estabelecida por esses checks.

Reprodução, a partir da pasta do curso:

```sh
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla projeto_agna/scripts/17_preparar_dados_eleicoes.R
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla projeto_agna/scripts/18_laboratorio_aula7_previsao_eleitoral.R
LC_ALL=pt_BR.UTF-8 LANG=pt_BR.UTF-8 Rscript --vanilla projeto_agna/scripts/19_validar_laboratorio_eleitoral.R
python3 projeto_agna/scripts/20_empacotar_laboratorio_eleitoral.py
```

## Bases adicionais com exterior

Em 13/10/2026, o script 17 recebeu a opção `--incluir-exterior`, que prepara dados observados de 27 UFs e ZZ. Os arquivos adicionais em `data/eleicoes_2022_2026/processed/` têm o sufixo `_com_exterior.csv`. São 2.637 zonas em 2022 e 2.641 em 2026. A preparação soma a zona do exterior e calcula seus percentuais sobre todos os votos válidos do turno.

O novo script 21 verifica os dados sem ajustar regressões. Retornou `VALIDACAO_DADOS_COM_EXTERIOR_OK`, conferindo chaves, ausências, denominadores, somas do exterior e igualdade da parte das 27 UFs com os CSVs anteriores. O total válido de 2026, incluindo exterior, coincide com a totalização nacional oficial. Relatório e reprodução em `data/eleicoes_2022_2026/docs/INCLUSAO_EXTERIOR.md`.

Esta adição abrange apenas a preparação e a validação dos dados observados. Não recalcula a modelagem nem as previsões. O laboratório e o ZIP anteriores continuam correspondendo à versão de 27 UFs.

## Revisão das condições de cálculo e consistência, 14/10/2026

O slide 9, antes intitulado “Condições para estimar o BLP”, foi substituído por “Calcular MQO na amostra”. Agora apresenta o critério de mínimos quadrados em dados observados finitos e distingue a existência de soluções da unicidade dos coeficientes. Posto completo de X permite usar a inversa de X'X. Observações i.i.d. e momentos populacionais finitos não são exigências para esse cálculo. Sem posto completo, há minimizadores, mas os coeficientes não são únicos.

O slide 10 apresenta a consistência a partir da convergência dos momentos amostrais Q_hat e q_hat para o alvo populacional Q e q, com Q invertível. Amostragem i.i.d. e segundos momentos finitos aparecem como um conjunto de hipóteses suficientes para essas convergências, pela lei dos grandes números. O texto explicita que elas também podem ocorrer com dependência ou distribuições distintas, sob condições próprias. A síntese, no slide 22, usa a mesma qualificação de suficiência. A formulação continua distinguindo o coeficiente populacional do BLP e a estimativa calculada na amostra.

Conferência conceitual: Hansen, Econometrics, capítulo 2 sobre a projeção linear, seção 3.24 sobre unicidade e colinearidade e seção 7.2 sobre consistência, na cópia local do curso. Para a extensão além de amostragem i.i.d., consultado Hansen e Lee (2019), “Asymptotic Theory for Clustered Samples”, Journal of Econometrics, DOI 10.1016/j.jeconom.2019.02.001, texto dos autores em https://arxiv.org/abs/1902.01497. Essa extensão fundamenta a ressalva do slide, sem acrescentar um novo tópico ao programa.

O Rmd foi recompilado com XeLaTeX. O PDF continua com 23 páginas. As páginas 9, 10 e 22 foram renderizadas e inspecionadas. A comparação do texto de todas as páginas com o PDF anterior no Git identificou mudanças somente nessas três páginas. A conferência das coordenadas de todas as palavras encontrou zero palavras fora das páginas. Os dados, scripts de simulação e figuras não foram alterados. Não foram executadas novas simulações, pois a revisão afeta a exposição das condições. O diff passou sem espaços finais nas linhas alteradas.

SHA-256 do PDF desta revisão: `2c55cee1aa3526a646c74d035e0bfed996bcca40e1ee068106de36e8606a2ba6`.

## Passagem didática da fórmula de MQO à consistência, 14/10/2026

Após o comentário do professor sobre a introdução abrupta de somatórios matriciais e da notação Q, o antigo slide 10 foi substituído por três etapas. O PDF passou de 23 para 25 páginas.

1. Slide 10: parte da fórmula conhecida `(X'X)^(-1)X'Y`, explica que dividir X'X por n multiplica sua inversa por n e mostra o cancelamento dos fatores ao dividir também X'Y por n. Explicita que a motivação é escrever o estimador em termos de médias amostrais para aplicar a lei dos grandes números.
2. Slide 11: escreve X'X e X'Y como uma matriz e um vetor de somas no caso de um preditor com intercepto. A divisão por n transforma essas entradas em médias. Define a média dos quadrados e a média dos produtos e liga essas quantidades à inclinação dada por covariância amostral dividida pela variância amostral.
3. Slide 12: aplica a lei dos grandes números aos produtos normalizados e apresenta o limite na notação de esperanças já usada para o BLP. Distingue os coeficientes definidos pela população da estimativa calculada por MQO na amostra. As hipóteses i.i.d. e de momentos finitos permanecem qualificadas como suficientes.

As letras Q e q foram retiradas da apresentação. A ponte usa os produtos matriciais que os alunos já estudaram. Conferência conceitual nas seções 3.3, 3.4 e 7.2 de Hansen, na cópia local do curso. A sequência mostra primeiro a equivalência algébrica, depois a interpretação das entradas e só então o limite probabilístico.

Verificação numérica das identidades com os pares `x = (1,2,4,5)` e `y = (2,1,5,7)`: a fórmula matricial e sua versão normalizada por n produziram os mesmos coeficientes. As expansões de X'X e X'Y e a igualdade da inclinação com as razões de momentos e de covariância/variância foram conferidas com tolerância de 10^-12. Resultado: `IDENTIDADES_ALGEBRICAS_MQO_OK`. Não houve nova simulação de Monte Carlo.

O Rmd foi recompilado com XeLaTeX. Os três novos slides foram renderizados e examinados, inclusive a versão final do slide 11 com as definições das médias. A comparação de texto confirmou que os 22 slides fora dessa sequência permanecem iguais, descontada a mudança de numeração. A conferência de coordenadas encontrou zero palavras fora das páginas. O diff passou sem espaços finais nas linhas alteradas.

SHA-256 do PDF desta revisão: `004a3fa931b1fa1b1993b18d3d7504021a35c0d12c3f8813ad16cce1f2bea102`.

## Destaque das suposições no slide 12, 14/10/2026

O slide 12 passou a apresentar, antes da lei dos grandes números, um bloco “Suposições da demonstração” com amostragem i.i.d. e segundos momentos finitos de Y e de cada preditor. A caixa “O que está sendo estimado?” e a nota final sobre outras condições foram removidas. As equações de convergência dos momentos e de consistência foram mantidas.

O Rmd foi recompilado com XeLaTeX. O PDF continua com 25 páginas. A comparação do texto com o PDF anterior identificou mudança somente no slide 12. A página final foi renderizada e examinada. A conferência das coordenadas de todas as palavras encontrou zero palavras fora das páginas, e o diff passou sem espaços finais nas linhas alteradas.

SHA-256 do PDF desta revisão: `cbf870c6000d31b5e18ce18970336804bf2f601999a0166cc2fb0f1e9b27f911`.

## Propriedade e suposição no slide 13, 14/10/2026

Os títulos das duas caixas passaram a ser “Propriedade do erro do BLP”, com E[x epsilon] = 0, e “Suposição chave para o não viés”, com E[epsilon | x] = 0. A frase explicativa sobre ortogonalidade por construção foi retirada, conforme a orientação de deixar essa explicação para a exposição oral. A suposição permanece no slide 13, antes da demonstração de não viés no slide 14.

O Rmd foi recompilado com XeLaTeX. O PDF continua com 25 páginas. A comparação de texto identificou alteração somente no slide 13. A página foi renderizada e examinada, com os títulos das duas caixas legíveis. A conferência das coordenadas de todas as palavras encontrou zero palavras fora das páginas, e o diff passou sem espaços finais nas linhas alteradas.

SHA-256 do PDF desta revisão: `8835be06991f3fb65646071e04ab8bfbded6e245c889d15580a49c32452d9c85`.

## Linearidade nos parâmetros e destaque da hipótese de não viés, 14/10/2026

No slide 13, a afirmação sobre a esperança condicional passou a explicitar linearidade nos parâmetros. A caixa da suposição contém apenas E[epsilon | x] = 0. Uma caixa separada, “Linear nos parâmetros”, liga essa hipótese à equação populacional e apresenta x = (1, X, X²)', com E[Y | X] = alpha + beta_1 X + beta_2 X². O texto distingue a linearidade nos coeficientes da possível curvatura em X. Esse exemplo substitui o exemplo anterior de erro quadrático. A afirmação de que ortogonalidade não implica média condicional zero permanece no slide.

No slide 14, a hipótese ganhou uma caixa própria antes da demonstração. Sob amostragem i.i.d., a média condicional zero de cada erro implica E[epsilon_vetor | X_matriz] = 0. A prova continua indicando por uma chave o termo que se anula. O fechamento sobre a esperança incondicional, tamanho de amostra fixo e dispensa de homocedasticidade foi mantido em texto compacto, permitindo destacar a hipótese e preservar espaço para o rodapé.

Conferência conceitual em Hansen, Econometrics, seção 2.16, “Linear CEF with Nonlinear Effects”, p. 31, e na demonstração de não viés da cópia local do curso. A distinção se aplica a transformações incluídas como preditores, como X².

O Rmd foi recompilado com XeLaTeX. O PDF continua com 25 páginas. As páginas 13 e 14 foram renderizadas e examinadas. A comparação de texto identificou alteração somente nesses dois slides. A conferência de coordenadas encontrou zero palavras fora das páginas, e o diff passou sem espaços finais nas linhas alteradas. Não houve mudança nos cálculos ou simulações.

SHA-256 do PDF desta revisão: `e0fc8b9a90facae6c915bcf62473d959ebd7b7823f8f90bb763f3f3dde57383a`.

## Substituição e distributiva na prova de não viés, 14/10/2026

O slide 14 passou a identificar onde se faz a substituição: na fórmula de MQO, Y é substituído por X beta + epsilon. A primeira linha da demonstração apresenta a fórmula original do estimador e a substituição. A segunda mostra a distributiva, com os termos (X'X)^(-1)(X'X) beta e (X'X)^(-1)X' epsilon. A terceira simplifica o primeiro termo para beta. A suposição chave de média condicional zero continua destacada no bloco azul, com a equação na mesma linha da implicação para acomodar as etapas algébricas.

O Rmd foi recompilado com XeLaTeX. O PDF continua com 25 páginas. A comparação do texto com o PDF anterior no Git identificou alteração somente no slide 14. A página foi renderizada e examinada, incluindo as três linhas da demonstração e o espaço para o rodapé. A conferência de coordenadas encontrou zero palavras fora das páginas, e o diff passou sem espaços finais nas linhas alteradas.

SHA-256 do PDF desta revisão: `20702abc0c0dce952c7ff18edb2be8641c17e5bd7de23690582ee8700a883642`.
