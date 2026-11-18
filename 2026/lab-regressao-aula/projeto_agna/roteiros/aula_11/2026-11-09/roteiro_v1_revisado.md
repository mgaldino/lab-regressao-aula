# Aula 11: interações e comparações condicionais

Versão 1 revisada para aprovação, preparada em 09/11/2026. Aula prevista para 25/11/2026. Público: pós-graduação, com regressão múltipla, indicadores de UF, covariância, intervalos, testes e resultados potenciais já apresentados. Proposta: manter 50 minutos de exposição e discussão e 75 minutos de laboratório, como na Aula 10. Slides ainda não montados.

## TL;DR

- Mensagem central: com interação, uma associação depende do valor de outra variável. É preciso dizer qual comparação está sendo feita, em que valor e com qual incerteza.
- Fio condutor: permitir inclinações diferentes para SP e RJ. O exemplo usa as 447 zonas dessas UFs presentes no treino da Aula 9; os modelos aditivo e interativo são reestimados nessa mesma subamostra. Os números da Aula 10, que usava as 27 UFs, não são transportados para a comparação entre especificações.
- Derivações completas por substituição e álgebra: duas retas a partir do indicador; diferença SP-RJ como função do T1; centralização em 40 p.p.; variância da combinação de coeficientes. As fórmulas e etapas pertencem ao percurso principal.
- Generalização curta para duas variáveis contínuas e esquema de quatro células para duas variáveis binárias. Distinguir heterogeneidade do efeito de um tratamento de interação causal entre duas intervenções, retomando as condições de identificação da Aula 4.
- Aplicação em R com `x_c * sp`, incluindo os termos individuais. Inferência com HC3 e referência t aproximada, suporte observado e interpretação associacional. Sem interações triplas, logit ou novos testes conjuntos.

\newpage

## Orçamento de tempo

As marcações FALA contêm o texto oral integral. Equações fora dessas marcações são apoios a desenvolver por etapas. Ações silenciosas incluem tempo adicional para escrever, substituir valores e examinar figuras; não duplicam o tempo da explicação escrita na fala. Interações com a turma são contabilizadas separadamente, uma única vez.

Referência provisória: 100 palavras/minuto; cenário conservador: 90. Depois de descontar a interação dos 50 minutos, reservar 15% do restante como folga. Não houve ensaio oral ou aplicação em sala. O laboratório tem orçamento próprio, de 75 minutos.

<!-- ORCAMENTO_INICIO -->
| Bloco | Palavras | Ações | Interação | Duração a 90 ppm | Acumulado |
|------------------------------------------|--------:|------:|---------:|----------------:|----------:|
| B1 — A restrição que o modelo aditivo impõe | 207 | 0:45 | 1:00 | 4:03 | 4:03 |
| B2 — Do produto entre variáveis às duas retas | 359 | 2:30 | 1:30 | 7:59 | 12:02 |
| B3 — Escolher uma origem que ajude a interpretar | 231 | 1:45 | 0:00 | 4:19 | 16:21 |
| B4 — A incerteza pertence à comparação escolhida | 371 | 2:30 | 1:30 | 8:07 | 24:29 |
| B5 — A mesma lógica com duas variáveis contínuas | 180 | 1:00 | 0:00 | 3:00 | 27:29 |
| B6 — O que seria uma interação causal? | 320 | 1:30 | 1:30 | 6:33 | 34:02 |
| B7 — Passar da pergunta ao código | 168 | 0:30 | 0:00 | 2:22 | 36:24 |

Fala: **1836 palavras**. A 90 palavras/minuto: 20:24 de fala + 10:30 de ações + 5:30 de interação = 36:24 antes da folga.

Folga: 6:41, correspondente a 15% dos 44,5 minutos de exposição disponíveis. Total conservador com folga: **43:05**. Saldo adicional: 6:56. A 100 palavras/minuto, com as mesmas ações, interação e folga: 41:02. Tempos estimados, sem ensaio; os segundos não expressam precisão empírica.
<!-- ORCAMENTO_FIM -->

\newpage

## B1 — A restrição que o modelo aditivo impõe

**Apoio visual previsto:** mesma escala de T1 e T2, comparação de duas retas paralelas com duas retas de inclinações diferentes. Indicar as distribuições observadas de X por UF. Não apresentar os números da subamostra como se fossem os do ajuste nacional anterior.

<!-- FALA -->
Na aula passada, comparamos São Paulo e Rio de Janeiro para o mesmo percentual de votos no primeiro turno. O modelo permitia níveis diferentes entre os estados, mas impunha uma inclinação comum. Portanto, a distância vertical entre as duas retas era a mesma em qualquer valor do primeiro turno. Essa constância vinha da especificação.

Hoje vamos permitir que a inclinação também varie. A pergunta será: a associação entre os dois turnos tem a mesma intensidade nos dois estados? Se as retas tiverem inclinações diferentes, a distância entre elas dependerá do ponto em que fizermos a comparação.

Para acompanhar a álgebra com duas categorias, vamos usar apenas as zonas de SP e RJ que já estavam no treino: trezentas e quinze de SP e cento e trinta e duas do RJ. Reestimaremos tanto o modelo aditivo quanto o modelo com interação nessas mesmas quatrocentas e quarenta e sete zonas. Assim, a comparação entre especificações mantém a amostra fixa. São resultados observados de dois mil e vinte e dois, com peso igual por zona.

Olhem os dois casos do gráfico. Em qual deles uma única diferença de nível resume a comparação entre os estados? Localizem duas posições no eixo horizontal e expliquem o que acontece com a distância vertical.
<!-- /FALA -->

**Ação e pausa:** 45 segundos para observar e apontar duas posições nas retas.

**Interação:** 1 minuto para responder à pergunta inicial.

## B2 — Do produto entre variáveis às duas retas

**Apoio visual previsto:** introduzir X, Y e D; substituir D=0 e D=1 em etapas; separar interceptos e inclinações; subtrair as duas expressões para obter a diferença de níveis. Mostrar a diferença entre uma inclinação e uma diferença de inclinações.

<!-- FALA -->
Chamaremos de X a porcentagem de votos de Lula no primeiro turno e de Y a porcentagem no segundo, ambas sobre todos os votos válidos de cada turno. D será um indicador: zero para RJ e um para SP. Acrescentamos ao modelo o produto de X por D, com seu próprio coeficiente, beta três.

Vamos chamar de m a função de previsão representada pelo modelo. Igualá-la à verdadeira esperança condicional exige que a forma esteja correta. Se ela for apenas uma aproximação, nossas comparações descrevem essa aproximação. O produto entre variáveis continua entrando linearmente nos coeficientes, portanto podemos estimar o modelo por mínimos quadrados.

Para o Rio, substituímos D por zero. Desaparecem o termo do indicador e o produto. A reta tem intercepto beta zero e inclinação beta um. Para São Paulo, substituímos D por um e agrupamos os termos. O intercepto passa a ser beta zero mais beta dois, e a inclinação passa a ser beta um mais beta três.

Logo, beta três é a diferença entre as inclinações de SP e RJ. Ele não é a inclinação de São Paulo. No nosso ajuste, as inclinações são aproximadamente um vírgula cento e dois no RJ e um vírgula zero cinquenta em SP. A diferença é menos zero vírgula zero cinquenta e um ponto percentual do segundo turno por ponto percentual do primeiro. As duas inclinações são positivas; a interação negativa indica que a de SP é menor.

Agora subtraímos a previsão do Rio da previsão de São Paulo no mesmo X. O intercepto comum e a inclinação de referência se cancelam. Sobra beta dois mais beta três vezes X. Essa é a diferença de níveis prevista, em pontos percentuais do segundo turno. Beta dois, sozinho, corresponde à comparação quando X é zero. E, quando X aumenta um ponto, essa diferença entre os estados muda em beta três.

Há duas leituras relacionadas: a inclinação depende do estado e a diferença entre estados depende do primeiro turno. Nenhuma delas, por si só, identifica uma transferência individual de votos ou um efeito causal. Antes de avançar, escrevam a inclinação de SP e a diferença SP menos RJ quando X vale quarenta.
<!-- /FALA -->

**Derivação completa prevista:**

\[
m(x,d)=\beta_0+\beta_1x+\beta_2d+\beta_3xd,
\qquad D=0\ (RJ),\quad D=1\ (SP).
\]

\[
\begin{aligned}
m(x,0)&=\beta_0+\beta_1x,\\
m(x,1)&=\beta_0+\beta_1x+\beta_2+\beta_3x\\
&=(\beta_0+\beta_2)+(\beta_1+\beta_3)x.
\end{aligned}
\]

\[
s_{RJ}=\beta_1,\qquad s_{SP}=\beta_1+\beta_3,
\qquad s_{SP}-s_{RJ}=\beta_3.
\]

\[
\begin{aligned}
\Delta(x)&=m(x,1)-m(x,0)\\
&=[\beta_0+\beta_2+(\beta_1+\beta_3)x]-[\beta_0+\beta_1x]\\
&=\beta_2+\beta_3x,\qquad \Delta(x+1)-\Delta(x)=\beta_3.
\end{aligned}
\]

**Ação e pausa:** 2min30s distribuídos entre substituições, agrupamentos e cancelamentos no quadro.

**Interação:** 1min30s para as duas expressões; respostas esperadas: beta um mais beta três; beta dois mais quarenta vezes beta três.

## B3 — Escolher uma origem que ajude a interpretar

**Apoio visual previsto:** manter as mesmas retas e deslocar apenas a origem de X para 40 p.p. Mostrar a substituição X=Xc+40 e os dois coeficientes que mudam. Não sugerir que centralizar acrescenta informação ou corrige a forma funcional.

<!-- FALA -->
O valor zero do primeiro turno está fora dos valores observados nessa amostra. Por isso, interpretar beta dois como diferença entre estados em zero exige extrapolação. Podemos escolher uma origem mais útil: quarenta por cento, um valor observado na região de suporte dos dois estados.

Definimos X centrado como X menos quarenta. Então X é X centrado mais quarenta. Substituímos essa expressão nos dois lugares em que X aparece e distribuímos os produtos. O novo intercepto, que chamarei de alfa zero, é beta zero mais quarenta vezes beta um. O novo coeficiente do indicador, alfa dois, é beta dois mais quarenta vezes beta três. As inclinações e sua diferença permanecem iguais.

Agora, X centrado igual a zero corresponde a quarenta por cento no primeiro turno. Alfa dois é diretamente a diferença SP menos RJ nesse ponto: aproximadamente zero vírgula seiscentos e oitenta e cinco ponto percentual. Em cinquenta por cento, X centrado vale dez. A diferença passa a ser alfa dois mais dez vezes beta três, aproximadamente zero vírgula cento e setenta e dois ponto.

Centralizar muda os coeficientes que representam níveis na origem, mas preserva as previsões, os resíduos e a comparação no mesmo valor original de X. Os números mudam de lugar na tabela porque mudamos a pergunta respondida pelo coeficiente do indicador. Também precisamos observar onde há dados dos dois grupos; a transformação não cria observações onde faltam.
<!-- /FALA -->

**Derivação completa prevista:**

\[
X_c=X-40,\qquad X=X_c+40,
\]

\[
\begin{aligned}
m(x,d)&=\beta_0+\beta_1(x_c+40)+\beta_2d+\beta_3(x_c+40)d\\
&=(\beta_0+40\beta_1)+\beta_1x_c+(\beta_2+40\beta_3)d+\beta_3x_cd\\
&=\alpha_0+\beta_1x_c+\alpha_2d+\beta_3x_cd.
\end{aligned}
\]

\[
\alpha_0=\beta_0+40\beta_1,\quad
\alpha_2=\beta_2+40\beta_3,\quad
\Delta(x)=\alpha_2+(x-40)\beta_3.
\]

**Ação e pausa:** 1min45s para distribuir os produtos e ligar os coeficientes aos pontos 40 e 50.

## B4 — A incerteza pertence à comparação escolhida

**Apoio visual previsto:** reutilizar a regra da Aula 10; expandir a variância de U+cV e substituir os coeficientes. Mostrar as duas estimativas de diferença com ICs pontuais e, separadamente, o teste de igualdade das inclinações. Não deduzir o teste da diferença pela sobreposição de bandas das duas retas.

<!-- FALA -->
Já sabemos calcular a comparação. Falta sua incerteza. Na aula passada, usamos a variância de uma diferença entre coeficientes. Agora precisamos da variância de uma soma ponderada. A mesma propriedade resolve o problema, mas precisamos carregar o peso da interação em todos os termos.

Chamemos de c o valor original do primeiro turno menos quarenta. Para uma comparação escolhida, c é fixo. U representa a estimativa de alfa dois, e V representa a estimativa de beta três. Ao centralizar U mais cV e elevar ao quadrado, aparecem a variância de U, c ao quadrado vezes a variância de V e duas vezes c vezes a covariância. Substituímos as três quantidades pelas entradas da mesma matriz estimada e tiramos a raiz.

Quando X é quarenta, c vale zero. A diferença é o próprio coeficiente do indicador, e seu erro-padrão vem da diagonal correspondente. Quando X é cinquenta, c vale dez: entram a variância da interação multiplicada por cem e a covariância multiplicada por vinte. Somar erros-padrão, ou ignorar essa covariância, não faz a mesma conta.

Usando HC3, a diferença em quarenta é aproximadamente zero vírgula seiscentos e oitenta e cinco, com erro-padrão de zero vírgula cento e quarenta e quatro. O intervalo de noventa e cinco por cento vai de aproximadamente zero vírgula quatrocentos e um a zero vírgula novecentos e sessenta e oito ponto. Em cinquenta, a diferença é zero vírgula cento e setenta e dois, e o intervalo vai de menos zero vírgula trezentos e noventa e dois a zero vírgula setecentos e trinta e seis.

Para esses intervalos, usamos a referência t com quatrocentos e quarenta e três graus de liberdade como aproximação. HC3 não corrige automaticamente dependência entre zonas. São intervalos pontuais para as comparações escolhidas, sem garantia simultânea para todos os valores de X.

Uma pergunta diferente é se as inclinações são iguais. Nesse modelo, ela corresponde a beta três igual a zero. O valor-p aproximado é zero vírgula zero vinte. Esse teste investiga paralelismo; o teste da diferença em cinquenta investiga igualdade dos níveis naquele ponto. Retas com inclinações diferentes podem se aproximar ou cruzar. E um intervalo que inclui zero não demonstra igualdade. Expliquem, em duplas, por que essas duas perguntas podem produzir conclusões diferentes.
<!-- /FALA -->

**Derivação completa da combinação; a distribuição de referência é retomada, sem nova prova:**

\[
c=x-40,\qquad \widehat\Delta(x)=\widehat\alpha_2+c\widehat\beta_3.
\]

\[
\begin{aligned}
\operatorname{Var}(U+cV)
&=E\!\left[\big((U-EU)+c(V-EV)\big)^2\right]\\
&=\operatorname{Var}(U)+c^2\operatorname{Var}(V)+2c\operatorname{Cov}(U,V).
\end{aligned}
\]

\[
\widehat{\operatorname{EP}}[\widehat\Delta(x)]
=\sqrt{\widehat C_{\alpha_2,\alpha_2}
+c^2\widehat C_{\beta_3,\beta_3}
+2c\widehat C_{\alpha_2,\beta_3}}.
\]

\[
IC_{95\%}(x)=\widehat\Delta(x)\pm t_{443,0{,}975}
\widehat{\operatorname{EP}}[\widehat\Delta(x)].
\]

**Ação e pausa:** 2min30s para a expansão, a substituição de c e a leitura dos dois intervalos.

**Interação:** 1min30s para distinguir igualdade de inclinações e de níveis.

## B5 — A mesma lógica com duas variáveis contínuas

**Apoio visual previsto:** uma fórmula geral e cortes em três valores de Z, descritos antes de qualquer gráfico. Mostrar a derivada em X e sua unidade. Não criar uma segunda aplicação empírica.

<!-- FALA -->
O indicador tornou a interpretação visível porque tínhamos apenas duas retas. A mesma lógica vale quando a segunda variável é contínua. Chamando essa variável de Z, o modelo inclui X, Z e o produto XZ. Para um valor fixo z, agrupamos os termos de X. A inclinação em X é beta um mais beta três vezes z.

Se Z aumenta uma unidade, essa inclinação muda em beta três. Também podemos inverter a leitura: a inclinação em Z depende de X. Aqui as unidades de X e Z podem ser distintas, por isso a unidade do coeficiente do produto precisa considerar as duas. A interpretação depende também da escala escolhida para Y.

O produto impõe uma forma específica: a inclinação em X varia linearmente com Z. Ele não representa toda heterogeneidade possível. Para apresentar o resultado, escolhemos valores substantivamente relevantes e observados de Z, calculamos as inclinações ou previsões e sua incerteza. Um valor médio de Z fora do suporte de um grupo não resolve o problema de comparação. Hoje manteremos o exemplo eleitoral com duas categorias para praticar essas contas.
<!-- /FALA -->

**Esquema algébrico previsto:**

\[
m(x,z)=\beta_0+\beta_1x+\beta_2z+\beta_3xz
=(\beta_0+\beta_2z)+(\beta_1+\beta_3z)x,
\]

\[
\frac{\partial m(x,z)}{\partial x}=\beta_1+\beta_3z,
\qquad \frac{\partial m(x,z)}{\partial z}=\beta_2+\beta_3x.
\]

**Ação e pausa:** 1 minuto para agrupar os termos e acompanhar a leitura das derivadas.

## B6 — O que seria uma interação causal?

**Apoio visual previsto:** quatro médias hipotéticas de comparecimento, duas intervenções binárias e a diferença entre duas diferenças. Distinguir esse exemplo dos dados eleitorais observados. Em seguida, exibir a definição do efeito de um tratamento dentro de um estrato anterior a ele.

<!-- FALA -->
Até aqui, descrevemos associações do modelo. Vamos retomar a aula de causalidade com um exemplo hipotético. T indica receber uma mensagem de mobilização, e V indica receber uma visita. Imagine um experimento que sorteie as duas intervenções, com possibilidade de receber nenhuma, apenas uma ou ambas. Suponhamos intervenções bem definidas, sem interferência entre pessoas e com probabilidade positiva nas quatro combinações.

Y de t e v representa o comparecimento que a pessoa teria sob aquela combinação. Para trabalhar diretamente em pontos percentuais, codificamos cem para comparecimento e zero para ausência. A tabela mostra médias hipotéticas: cinquenta sem intervenção, cinquenta e dois só com mensagem, cinquenta e cinco só com visita e sessenta com ambas. São números construídos para a explicação.

Sem visita, a mensagem eleva o comparecimento médio em dois pontos. Com visita, eleva em cinco. A diferença entre esses dois efeitos é três pontos percentuais: uma interação causal positiva na escala aditiva. O sorteio das duas intervenções permite identificar as quatro médias por suas contrapartidas observadas, sob as condições mencionadas. Em uma regressão saturada das duas variáveis binárias e seu produto, essa diferença entre diferenças corresponde ao coeficiente do produto.

Agora considerem outra pergunta. Sorteamos apenas a mensagem e comparamos seu efeito entre grupos definidos por uma característica medida antes do tratamento, como interesse político. Podemos identificar o efeito da mensagem dentro de cada grupo, desde que haja tratados e controles em ambos. O efeito pode variar com a característica, sem que tenhamos identificado o efeito de mudar essa característica. Chamamos isso de modificação ou heterogeneidade do efeito do tratamento.

Assim, incluir um produto na regressão especifica uma comparação estatística. Para interpretá-la causalmente, precisamos definir as intervenções ou os estratos e justificar a identificação. No exemplo SP e RJ, não sorteamos estado nem votação no primeiro turno. Portanto, a comparação continua associacional. Em duplas, indiquem o que foi sorteado em cada exemplo e qual efeito esse desenho permite identificar.
<!-- /FALA -->

**Tabela 1. Médias hipotéticas de comparecimento sob duas intervenções, em porcentagem. Números construídos para a aula, sem dados de pesquisa.**

| | Sem visita, V=0 | Com visita, V=1 |
|---|---:|---:|
| Sem mensagem, T=0 | 50 | 55 |
| Com mensagem, T=1 | 52 | 60 |
| Efeito da mensagem, p.p. | 2 | 5 |

**Esquema formal: quatro médias, comparação e condições de identificação:**

\[
I=E[Y(1,1)-Y(0,1)]-E[Y(1,0)-Y(0,0)]
=(60-55)-(52-50)=3\text{ p.p.}
\]

O esquema usa atribuição conjunta independente dos resultados potenciais, positividade nas quatro células e consistência, incluindo intervenções bem definidas e ausência de interferência. Aqui Y(t,v) pertence a {0,100}, de modo que suas médias têm a escala da tabela. Na codificação alternativa 0/1, as médias seriam 0,50; 0,52; 0,55 e 0,60, e I=0,03 equivaleria a 3 p.p.

Para uma característica pré-tratamento Z e somente T sorteado:

\[
\tau(z)=E[Y(1)-Y(0)\mid Z=z]
=E[Y\mid T=1,Z=z]-E[Y\mid T=0,Z=z].
\]

Essa igualdade requer identificação de T dentro dos estratos, positividade e consistência; não identifica uma intervenção em Z. A demonstração geral de identificação não é refeita nesta aula.

**Ação e pausa:** 1min30s para localizar as quatro células e calcular os dois efeitos e sua diferença.

**Interação:** 1min30s para distinguir as intervenções e a característica prévia.

## B7 — Passar da pergunta ao código

**Apoio visual previsto:** três linhas de R, ligadas a termos da equação; sequência pergunta, valor do moderador, comparação e intervalo. Manter a fala completa nas notas, não transcrevê-la nos slides.

<!-- FALA -->
No R, criaremos o indicador de São Paulo e a variável do primeiro turno centrada em quarenta. Na fórmula, X centrado asterisco SP inclui os dois termos individuais e o produto. O asterisco tem essa função na fórmula de um modelo. Não substituímos a especificação completa apenas pelo produto: isso imporia restrições adicionais sobre as retas.

No laboratório, vamos ajustar os modelos aditivo e interativo na mesma subamostra, conferir as retas e calcular a diferença entre os estados em quarenta e cinquenta. Usaremos a matriz HC3 para os intervalos e verificaremos a comparação mudando o centro da variável. Os gráficos deverão mostrar onde há dados e qual quantidade está sendo comparada.

Ao escrever a conclusão, incluam o valor do primeiro turno, a diferença prevista, sua unidade, o intervalo e os limites da inferência. A mensagem da aula é que a tabela de coeficientes precisa ser traduzida em uma comparação definida. Com interação, precisamos completar a frase: associação de qual variável, para qual grupo ou valor da outra variável?
<!-- /FALA -->

```r
dados$sp <- as.integer(dados$uf == "SP")
dados$x_c <- dados$pct_lula_t1 - 40
modelo <- lm(pct_lula_t2 ~ x_c * sp, data = dados)
# x_c * sp equivale a x_c + sp + x_c:sp na fórmula.
```

**Ação e pausa:** 30 segundos para apontar as colunas correspondentes aos termos do modelo.

## Laboratório proposto: 75 minutos

| Etapa | Minutos | Produto |
|---|---:|---|
| Recuperar o treino RJ-SP e conferir suporte | 10 | Unidade, denominador e distribuição de X |
| Ajustar modelos aditivo e interativo | 15 | Duas retas e interpretação dos coeficientes |
| Centralizar e conferir a invariância | 10 | Mesmas previsões, nova interpretação da origem |
| Calcular diferenças e ICs HC3 em 40 e 50 | 20 | Comparações com a covariância correta |
| Gráficos, conclusão e comparação em duplas | 15 | Figura e interpretação associacional |
| Organizar a entrega | 5 | Código, resultados e texto |

O laboratório será derivado do roteiro aprovado. O script de conferência do exemplo já existe; ele não é o roteiro discente nem monta figuras ou slides.

\newpage

## Fontes e limites da preparação

- Programa local: Aula 11 de interações em 25/11/2026. Aronow e Miller (2019), seção 4.3.3, permanece como leitura principal indicada no programa; essa seção não foi consultada integralmente nesta preparação.
- Shalizi, *The Truth About Linear Regression*, capítulo 17, especialmente seções 17.2, 17.3 e 17.5: produto entre preditores, interpretação condicional e implementação. [Texto disponibilizado pelo autor](https://www.stat.cmu.edu/~cshalizi/TALR/TALR.pdf). Consulta em 09/11/2026. O exemplo eleitoral, as contas e as verificações são próprios desta aula.
- [Documentação oficial de fórmulas do R](https://www.stat.math.ethz.ch/R-manual/R-devel/library/stats/html/formula.html): interpretação de `*` e `:` nas fórmulas. Consulta em 09/11/2026.
- VanderWeele (2009), [*On the distinction between interaction and effect modification*](https://pubmed.ncbi.nlm.nih.gov/19806059/), e Knol e VanderWeele (2012), [*Recommendations for presenting analyses of effect modification and interaction*](https://pmc.ncbi.nlm.nih.gov/articles/PMC3324457/): distinção entre uma intervenção com efeito heterogêneo e duas intervenções. A tabela de mobilização foi criada para a aula e não é resultado desses trabalhos.
- Dados observados: subconjunto RJ-SP do [treino de 2022 preservado na Aula 9](../../../output/aula_09/resultados_aula_09.rds), conferido contra o [CSV original de zonas de 2022](../../../data/eleicoes_2022_2026/processed/presidente_2022_zonas.csv). Sem coleta nova; porcentagens sobre todos os votos válidos do respectivo turno, peso igual por zona. A proveniência original está em [SOURCES.yaml](../../../data/eleicoes_2022_2026/docs/SOURCES.yaml).
- `verificar_exemplo.R` confere centralização, matriz de covariância, regressões por grupo, diferenças de previsões e erros-padrão por mudança do centro. Resultados em `numeros_verificados_v1.json`. São verificações de execução e álgebra, sem validação empírica da independência entre zonas ou da forma funcional.
- `verificar_roteiro.py` conta a fala e calcula o tempo. A revisão pedagógica própria está em `revisao_pedagogica_v1.md`; não houve revisão independente, ensaio oral ou montagem do deck.
