# Nota de entrega — Aula 5

## Escopo confirmado no syllabus

- Data: 23 de setembro de 2026.
- Tema: Regressão I — estimação por mínimos quadrados ordinários, caso bivariado e interpretação.
- Leituras: Aronow e Miller, capítulo 4, seção 4.1; Hansen, seções 3.3 a 3.8.
- Conhecimentos acumulados: probabilidade e simulação; esperança, variância, covariância, correlação e EQM; CEF, melhor preditor e BLP; resultados potenciais, identificação, atribuição aleatória, ignorabilidade, viés de seleção, variáveis pós-tratamento e distinção entre descrição, associação, previsão e causalidade.
- Produto AGNA: modelo linear simples e interpretação da associação entre o ano e a convergência média anual dos votos de Brasil e China.

## Arquivos entregues

- `projeto_agna/laboratorios/slides_aula_05_mqo_bivariado.Rmd`
- `projeto_agna/output/pdf/slides_aula_05_mqo_bivariado.pdf`
- `projeto_agna/scripts/06_laboratorio_aula5_mqo_bivariado.R`
- `projeto_agna/output/aula_05/hashes_protegidos_antes.txt`
- `projeto_agna/output/aula_05/hashes_protegidos_depois.txt`

## Fluxo do laboratório

O script lê diretamente o único banco autorizado, valida 1.762 resoluções, agrega explicitamente para 20 anos, preserva denominadores e numeradores, cria `anos_desde_1997`, constrói o gráfico em quatro etapas, calcula manualmente os coeficientes com covariância e variância, estima a mesma reta com `lm()`, compara os resultados, recupera valores ajustados e resíduos e encerra com interpretação, exercício de recentralização em 2009 e limites causais.

Resultados descritivos: intercepto de 0,767151656, interpretado como 76,715% previstos em 1997, e inclinação de 0,003891086, equivalente a associação de +0,389 ponto percentual por ano. Os 20 anos recebem peso igual, embora seus denominadores sejam diferentes.

## Testes e revisão

- Syllabus e leituras: PASS.
- Derivação de MQO: PASS.
- Conexão entre BLP populacional e MQO amostral: PASS.
- Unidades e interpretação dos coeficientes: PASS.
- Separação entre associação e causalidade: PASS.
- Ausência de inferência antecipada: PASS.
- Simplicidade e legibilidade do script: PASS.
- Numeração, legendas, unidades, denominadores e fontes: PASS.
- Compilação XeLaTeX e inspeção visual das 44 páginas da entrega inicial: PASS.
- Parse e execução integral do script: PASS.
- Coeficientes manuais e de `lm()` iguais com tolerância de 10^-12: PASS.
- Inexistência de `Rplots.pdf` e outputs acidentais: PASS.
- Integridade de autoria dos materiais anteriores: PASS; nenhuma frente da Aula 5 editou arquivos anteriores. A comparação byte a byte encontrou uma mudança concorrente da Aula 4, registrada abaixo e preservada sem restauração.

Warnings visíveis: `dplyr` e `ggplot2` foram compilados sob R 4.4.3, enquanto o runtime usado foi R 4.4.2. As mensagens normais de masking permaneceram visíveis. Não houve warning substantivo de dados, estimação ou compilação.

## Limitações e divergências

- A pasta `2026/lab-regressao-aula` não contém `.git` e não é reconhecida como repositório; portanto, não foi possível auditar `git status` ou produzir diff Git local. Nenhum commit ou push foi realizado.
- O syllabus, os dados e o produto da Aula 5 são consistentes: a base permite exatamente 20 observações anuais entre 1997 e 2016.
- Durante o trabalho, ocorreu um fluxo autoral concorrente da Aula 4. O arquivo `projeto_agna/scripts/05_laboratorio_aula4_causalidade.R` mudou entre os registros de hashes, e surgiram `slides_aula_04_causalidade_identificacao.Rmd` e seu PDF. Esses arquivos não pertencem à entrega da Aula 5, não foram editados pelas duas frentes e foram preservados no estado encontrado.
- O modelo descreve associação linear em uma amostra de 20 anos; não identifica efeito do tempo, de 2009, do comércio ou da China e não sustenta extrapolação além de 1997–2016.

## Atualização pedagógica de 22 de setembro de 2026

Após o parecer pedagógico e a discussão em sala planejada sobre o princípio plug-in, o arquivo-fonte e o PDF dos slides foram atualizados. A versão atual tem 54 páginas e começa com a regra intuitiva, a média e a variância plug-in, o contraste entre viés e consistência, condições de regularidade e a definição formal $\widehat\theta=T(\widehat F)$, antes de introduzir regressão.

- O gráfico dos 20 pontos anuais, ainda sem reta, abre a parte de regressão; a diferença entre os extremos observados e a inclinação do MQO é retomada após o ajuste.
- Os quatro pares do exemplo aparecem antes da derivação; duas pausas curtas interrompem o bloco algébrico e a pausa sobre recentralização foi deslocada para junto da explicação correspondente.
- As tabelas passaram a usar vírgula decimal. O ano bruto recebeu a notação $A_i$, distinta de $X_i=A_i-1997$. O gráfico da AGNU usa eixo de 0% a 100% e distingue os 20 anos dos denominadores anuais das taxas, que variam de 61 a 112 resoluções.
- A chamada de `lm()` nos slides usa os mesmos nomes de objetos e variáveis do roteiro do laboratório. A atividade final de interpretação passou a explicitar as unidades, e um slide indica três momentos para discutir as saídas práticas.

A atualização foi compilada com R Markdown e XeLaTeX. Foram conferidos o texto extraído do PDF, a paginação e a legibilidade das páginas alteradas. Os valores de 1997 (79,3%), 2016 (69,6%) e da inclinação (+0,39 ponto percentual por ano) foram confrontados com a base da aula. O roteiro do laboratório não foi modificado nesta atualização.

## Cortes e revisão de conteúdo de 23 de setembro de 2026

A versão anterior, com 54 páginas, foi preservada na tag Git `aula05-v1` (`git show aula05-v1:./projeto_agna/output/pdf/slides_aula_05_mqo_bivariado.pdf`). A versão atual tem 37 páginas.

- Todas as menções a causalidade, identificação e contrafactual foram removidas dos slides. O tema fica para a fala do professor.
- O antigo "Mapa conceitual" virou o slide "Roteiro", logo após a capa, com problema, ponte com a Aula 3, roteiro em cinco blocos e objetivo da aula.
- O slide do MQO como estimador plug-in do BLP passou a mostrar também $\operatorname{V}(X)\mapsto n^{-1}\sum_i(X_i-\bar X)^2$ e absorveu a pergunta sobre o fator $1/n$.
- Os títulos foram reescritos como expressões nominais curtas.
- Slides cortados: estimação vs. identificação; três elementos; pausa sobre a fórmula; reta alternativa em texto (a figura mostra as duas SQR); centralização em $X=2$; pausa de recentralização em 2009 (mantida no roteiro do laboratório); inclinação e causalidade; propriedades dos resíduos; cinco erros.
- Fusões: reta com valor ajustado e resíduo; SQR, quadrado e argmin; função objetivo com as duas condições de primeira ordem; inclinação com intercepto no exemplo; tabela de resíduos com a checagem das equações normais; valores ajustados com extrapolação.
- Mantidos por decisão do professor: condições de regularidade do plug-in e comparação população vs. amostra.

Compilação XeLaTeX sem erro. Inspeção visual das 37 páginas, com ajuste dos slides 19 e 22, que encostavam no rodapé. Conferidos no texto extraído os números 76,7%, 0,7672, 0,0039, 0,39 p.p., 79,3%, 69,6% e SQR 4,20. O roteiro do laboratório não foi modificado.

## Exercício de ajuste visual (23 de setembro de 2026, tarde)

O slide com a série Brasil–China no eixo de 0% a 100% saiu. Entraram quatro rodadas de ajuste visual, cada uma com dois slides idênticos: o primeiro só com os pontos, para os alunos desenharem na lousa a reta que melhor se ajusta; o segundo com a reta de MQO. Os dois gráficos de cada par têm os mesmos eixos e o mesmo tamanho, e a diferença de pixels entre eles fica restrita à área de plotagem.

| Rodada | Dados | n | r | Inclinação de MQO |
|---|---|---:|---:|---:|
| 1 | Concordância com EUA × concordância com China, AGNU 2019 (sessão 74), por país | 191 | −0,92 | −1,09 |
| 2 | Concordância com EUA × concordância com Rússia, AGNU 2018 (sessão 73), por país | 191 | −0,70 | −0,46 |
| 3 | Concordância com Rússia × concordância com Brasil, AGNU 2019, por país | 191 | 0,64 | 0,63 |
| 4 | Convergência Brasil–China por ano, 1997–2016 (base AGNA), eixo de 65% a 90% | 20 | 0,39 | +0,39 p.p./ano |

As rodadas 1 a 3 leem `data/raw/ideal_points/IdealpointestimatesAll_Jun2024.csv` (colunas `USAgree`, `ChinaAgree`, `RUSSAgree` e `BrazilAgree`; ano = sessão + 1945). Cada rodada exclui os dois países que definem os eixos. O gráfico "AGNA: taxa anual e reta de MQO" passou a usar o mesmo eixo vertical de 65% a 90% da rodada 4. Os slides agora têm 44 páginas.

Nas rodadas 1 e 2, o Brasil aparece destacado em verde e rotulado nos dois slides de cada par: em 2019, 26,4% de concordância com os EUA e 60,7% com a China; em 2018, 18,9% com os EUA e 66,3% com a Rússia. Na rodada 3, o Brasil define o eixo vertical e não é um ponto; na rodada 4, todos os pontos são do Brasil.

## Laboratório: gráficos da lousa, correlação e padronização

O roteiro `scripts/06_laboratorio_aula5_mqo_bivariado.R` ganhou a seção 10. Com isso, o exercício de recentralização passou a ser a seção 11 e os limites, a 12. A seção 10 refaz os quatro gráficos da lousa (Figura 2, com o Brasil em verde nas relações 1 e 2) e segue três passos: correlação por relação, `lm()` na relação 1 e regressão com X e Y padronizados na relação 1. Depois repete os passos nas quatro relações em `comparacao_relacoes` e mostra a Figura 3, com X e Y padronizados, a reta de MQO e a reta de correlação perfeita com o mesmo sinal.

| Relação | r | Inclinação | Inclinação padronizada |
|---|---:|---:|---:|
| EUA × China, 2019 | −0,915 | −1,090 | −0,915 |
| EUA × Rússia, 2018 | −0,696 | −0,456 | −0,696 |
| Rússia × Brasil, 2019 | 0,636 | 0,627 | 0,636 |
| Ano × convergência Brasil–China | 0,394 | 0,389 | 0,394 |

Execução integral do script sem erro. As duas verificações (inclinação = r × dp(Y)/dp(X); inclinação padronizada = r) retornam TRUE, e os gráficos foram conferidos visualmente. O slide "Laboratório" passou a listar esse quarto momento.

O slide "Equações normais" passou a mostrar a origem das duas condições (as derivadas da SQR em relação a $a$ e a $b$ igualadas a zero, uma por coeficiente) e a razão do nome: em geometria, *normal* quer dizer perpendicular, e as duas somas são produtos internos nulos do vetor de resíduos com $\mathbf 1_n$ e com $\mathbf X$. O Hansen usa o termo (eq. 2.21 para o BLP) sem explicar a origem; a leitura geométrica é a explicação usual, e a origem histórica do termo não foi verificada.

As perguntas aos alunos saíram do slide "Equações normais". Um slide novo, "Equações normais: população e amostra", mostra as condições do BLP ($E[\varepsilon]=0$, $E[X\varepsilon]=0$) ao lado das amostrais e as apresenta como aplicação do princípio plug-in. Ele registra também que as condições do BLP valem por construção, com segundos momentos finitos e $\operatorname{V}(X)>0$. Os slides agora têm 45 páginas.

Notação unificada segundo a convenção do professor: letra grega sem chapéu para parâmetros populacionais e com chapéu para estimativas amostrais. Os argumentos $a$, $b$ saíram de todos os slides. O critério, as condições de primeira ordem e as equações normais agora são escritos em $\widehat\alpha$, $\widehat\beta$ na amostra e em $\alpha$, $\beta$ na população, e o $\arg\min$ com variáveis mudas foi substituído por uma definição em palavras. A convenção é enunciada no slide "Valor ajustado e resíduo", primeiro uso de $\widehat\alpha$.

Candidatos da minimização com til: $(\widehat\alpha,\widehat\beta)=\arg\min_{(\widetilde\alpha,\widetilde\beta)}\operatorname{SQR}(\widetilde\alpha,\widetilde\beta)$ na amostra e $(\alpha,\beta)$ como o par $(\widetilde\alpha,\widetilde\beta)$ que minimiza $E[(Y-\widetilde\alpha-\widetilde\beta X)^2]$ na população. As derivadas são tomadas em $\widetilde\alpha,\widetilde\beta$ e iguais a zero no mínimo $(\widehat\alpha,\widehat\beta)$. A frase "as duas derivadas se anulam", que sugeria cancelamento mútuo, passou a "são iguais a zero".

O slide "Pergunta do projeto AGNA" foi cortado. O rótulo "AGNA" saiu de todo texto visível dos slides. Títulos: "Centralização em 1997", "Brasil e China: reta de MQO" e "Brasil e China: coeficientes". No roteiro da aula, o item passou a "Aplicação: votos de Brasil e China na AGNU". As fontes agora citam as votações nominais da AGNU (pacote unvotes) e os dados de Voeten, Strezhnev e Bailey. Os slides têm 44 páginas.
