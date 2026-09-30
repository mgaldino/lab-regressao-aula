# Revisão: slides da Aula 6 (regressão múltipla)

- **Arquivos**: `projeto_agna/laboratorios/slides_aula_06_regressao_multipla.Rmd` e `projeto_agna/output/pdf/slides_aula_06_regressao_multipla.pdf` (26 páginas); laboratório `projeto_agna/scripts/09_laboratorio_aula6_regressao_multipla.R`.
- **Versão revisada**: entrega do subagente do Codex às 15:03 de 29/09/2026. Rmd SHA-256 `cb8f19035c25…`; PDF `aafdc9eaaec9…`; laboratório `db2ca978a9d7…`. Depois disso, o Codex principal editou o laboratório para não gerar `Rplots.pdf` (hash `9f0b0c6ddc96…` às 15:07).
- **Aula**: 30/09/2026. Syllabus: regressão múltipla, interpretação condicional, preditores categóricos e viés de variável omitida; leituras Hansen, seções 2.21–2.25, e Shalizi, cap. 12 e seção 14.3.
- **Formato do encontro**: 2 h de teoria + 1h30 de laboratório em R.
- **Ritmo de referência**: decks de 44–46 páginas levaram quase 4 h, cerca de 11 páginas por hora. Meta para a teoria: 20–24 páginas.
- **Skill**: slide-excellence, adaptada para aula de curso (a sequência Puzzle → Literatura → Argumento… não se aplica).

## Avaliação geral

| Dimensão | Nota | Comentário |
|---|---|---|
| Narrativa | C− | Ordem trocada: símbolos (5), categórica (6) e variável omitida (7–9) vêm antes do critério de MQO (10). A aplicação (11–21) não usa as ferramentas da primeira parte: o slide 24 pergunta por que o coeficiente passa de 0,80 a 1,69, e o deck não deu a fórmula que responde. Falta motivação substantiva para a pergunta comércio–votos. |
| Conteúdo técnico | D+ | Os números estão certos e reproduzíveis. O conteúdo das leituras quase não aparece: sem BLP com vários preditores, sem equações normais com k preditores, sem decomposição do coeficiente (Hansen 2.23), viés de variável omitida só em exemplo numérico, categórica só binária. Notação diverge da Aula 5. |
| Design visual | C+ | Limpo e legível. Figura 1 com o rótulo do eixo y cortado e título interno que repete o título do slide; Figura 2 liga especificações com uma linha e depois precisa avisar que a linha não é tempo; legendas repetitivas. |
| Calibração para a turma | D+ | Para pós-graduação, a parte teórica tem 8 slides e nenhuma derivação. Onze slides tratam de conferência de dados, escala e ressalvas. |
| Timing | B− | 26 páginas dão cerca de 2h20 no seu ritmo, perto da meta. A distribuição está errada: 8 slides de teoria, 11 de aplicação e ressalvas, 3 de instruções de laboratório, 4 de abertura e fechamento. O slide 2 anuncia 65 min de exposição e 45 de laboratório, que não é o formato do curso. |
| Aderência às suas regras | D | 9 avisos causais nos slides e 2 no script; `synth_data.rds` citado como fonte 4 vezes; título-tese (17); blocos que falam com a turma; negações de espantalho; "desfecho" no lugar de "variável resposta"; $u_i$ no lugar de $\varepsilon$. |
| **Geral** | **C−** | Tamanho adequado e contas corretas. A teoria da aula precisa ser escrita, e a aplicação precisa de outro preditor ou de outra apresentação. |

## Diagnóstico

### 1. A teoria da regressão múltipla ficou de fora

As seções 2.21–2.25 do Hansen cobrem os coeficientes da projeção com vários regressores (2.21), os subvetores (2.22), a decomposição do coeficiente (2.23), o viés de variável omitida com regressão curta e longa (2.24) e a melhor aproximação linear (2.25). No deck:

- **BLP com vários preditores.** O slide 5 fala em "coeficiente da projeção linear de referência" sem dizer qual minimização define $\beta_j$. O BLP da Aula 3 não é generalizado.
- **Equações normais.** A Aula 5 tem o slide "Equações normais: população e amostra" ($E[\varepsilon]=0$, $E[X\varepsilon]=0$ e seus análogos amostrais) e apresenta o MQO como plug-in do BLP. A Aula 6 não mostra as equações com $k$ preditores e não usa o princípio plug-in. O fio população → plug-in → amostra, que organiza as Aulas 3 e 5, some.
- **Interpretação condicional.** O slide 4 mostra $\widehat Y(X+1,Z)-\widehat Y(X,Z)=\widehat\beta_1$, que é a forma funcional aditiva. O que "manter $Z$ constante" faz com os dados vem da decomposição do coeficiente (Hansen 2.23; Frisch–Waugh–Lovell na amostra): $\widehat\beta_1$ é a inclinação de $Y$ sobre a parte de $X_1$ que não é prevista linearmente por $X_2$. O deck não traz esse resultado, que é o conteúdo central de "interpretação condicional" no syllabus.
- **Viés de variável omitida.** Aparece só como conta no exemplo de quatro unidades ($5=2+3\times1$). A fórmula geral $\gamma_1=\beta_1+\beta_2\delta$, com $\delta=\operatorname{Cov}(X_1,X_2)/\operatorname{V}(X_1)$, sai em três linhas da condição $\operatorname{Cov}(X_1,\varepsilon)=0$ da regressão longa. A tabela de sinais, que é a ferramenta que os alunos usam em artigos, também falta.
- **Preditores categóricos.** Uma única indicadora (América Latina). Com duas categorias, a referência é trivial. Faltam $k$ categorias e $k-1$ indicadoras; a armadilha das indicadoras, que é a condição de posto do $(\mathbf X^\top\mathbf X)^{-1}$ citada em nota de rodapé no slide 10; a troca de referência, que altera os coeficientes e preserva os valores ajustados; e a ligação com a Aula 3 ("Indicadoras de ano saturam o BLP"): só com a categórica, o MQO reproduz as médias por grupo.

### 2. A aplicação gasta a aula com um preditor sem unidade e uma amostra mal descrita

- **Preditor sem unidade.** Em `synth_data.rds`, `perc_trade_with_china` e todas as covariáveis contínuas usadas (comércio com EUA, hiato de poder, `pci_cur`, `CA_GDP`) têm média 0 e desvio-padrão 0,5 exatos nos 1.920 país-anos, que é a padronização por dois desvios-padrão de Gelman (2008). A escala original não está no repositório. O deck repadroniza em 2016 e lê "p.p. de convergência por um desvio-padrão do campo comercial transformado". Numa aula sobre interpretação de coeficientes, o coeficiente central fica sem leitura substantiva, e os slides 13, 16, 21 e 23 gastam tempo explicando a escala. O slide 13 diz que "o dicionário local não documenta a transformação"; a transformação é identificável pelos momentos, e o que falta é a variável original.
- **Amostra.** Os 96 países são o Brasil e os 95 países do grupo de doadores de um desenho de controle sintético, com `donor_nunca_china_principal = 1`: países em que o indicador de tratamento (China como principal destino comercial) é zero em todo 1997–2016. O Brasil é o único tratado em 2016. O deck chama a amostra de "96 países da AGNU". A regressão de convergência em comércio com a China descreve, portanto, os países menos ligados comercialmente à China. Isso precisa estar no slide da aplicação.
- **Covariável de medida.** O M2 inclui o número de pares de votos válidos ("cobertura") como preditor. É o denominador da taxa; seu coeficiente não tem leitura substantiva e confunde o bloco de controles.
- **Hiato de poder.** Também padronizado, e o deck não diz como é medido nem o que significa um valor maior.
- **Motivação.** O deck não diz por que comércio com a China se associaria a votos na AGNU. Uma referência direta é Flores-Macías e Kreps (2013, *Journal of Politics* 75(2)), sobre comércio com a China e convergência de votos na AGNU em países da África e da América Latina.

### 3. O deck contraria regras que você já deu

- **Avisos causais** (regra da Aula 5, válida para as aulas de regressão): slide 4 ("Leitura correta"), 9 ("Cuidado"), 11 ("A taxa não é uma medida de alinhamento causal nem um resultado de intervenção"), 14 (legenda "Associação, sem leitura causal"), 20 ("Limite"), 21 ("coeficientes são associações entre países"), 24 (pergunta 4 e "parágrafo de interpretação associacional"), 25 (item 3). No script: linha 7, legenda da linha 218 e pergunta e).
- **Fonte interna** `synth_data.rds` nos slides 11, 14, 17 e 26, com "painel didático preservado" e "manifesto local". O aluno não sabe o que são. A fonte deve ser a origem real dos dados.
- **Títulos**: "O coeficiente muda entre especificações" (17) é frase-tese; "Como o MQO escolhe os coeficientes" (10) é pergunta ("Critério de MQO"); "A regressão curta e a comparação condicional" (8) é longo.
- **Blocos que falam com a turma**: "Pergunta para a turma" (7), "Unidade a escrever na resposta" (23), "Leitura correta" (4), "Mudança na pergunta" (3).
- **Negações de espantalho**: slide 11 (acima); "A linha não representa tempo nem incerteza" (17), necessária só porque a figura liga especificações com uma linha.
- **Vocabulário**: "desfecho" (5, 20). Na Aula 4 a turma adotou "variável resposta".
- **Notação**:
  - a Aula 5 usou $\widehat\alpha$, $\widehat\beta$, $\varepsilon$ e $\widehat e_i$; a Aula 6 passa a $\widehat\beta_0,\widehat\beta_1,\widehat\beta_2$ e introduz $u_i$ para o desvio populacional (slide 5) sem aviso. No Hansen 2.24, $u$ é justamente o erro da regressão curta;
  - $Z$ é o segundo preditor nos slides 3, 4 e 7–9 e o escore padronizado no slide 13 ($Z_{Xi}=(X_i-\bar X)/s_X$);
  - $X$ vira "todos os outros preditores" no slide 18;
  - $\widehat\gamma$ aparece para a indicadora (slide 6) sem aviso, enquanto o slide 5 indexa todos os coeficientes por $\beta_j$;
  - a equação populacional tem índice $i$ (slide 5); a Aula 5 escreveu a população sem índice ($Y$, $X$, $\varepsilon$).
- **Tempo no slide 2**: "Exposição e discussão: cerca de 65 minutos. Laboratório: cerca de 45 minutos."

## Top 5 ações de maior impacto

1. **Escrever o bloco teórico na ordem população → amostra → interpretação**: BLP com dois preditores e três condições de primeira ordem; equações normais amostrais como plug-in e forma matricial com a condição de posto; interpretação condicional pela decomposição do coeficiente, com a derivação de três linhas. No exemplo de quatro unidades, o resíduo de $X$ sobre $Z$ é $(0;\,-0{,}5;\,0{,}5;\,0)$. Só B e C, que têm o mesmo $Z$, pesam, e $\widehat\beta_1=1/0{,}5=2$. É a comparação B–C que o slide 8 faz informalmente.
2. **Dar ao viés de variável omitida a fórmula geral**, com a derivação a partir das equações normais, a tabela de sinais e uma aplicação com unidades reais. Nos dados Brasil–China por resolução, a inclinação do ano é +0,31 p.p./ano; com a indicadora de resoluções sobre o conflito palestino, +0,45 p.p./ano; e $0{,}31=0{,}45+23{,}2\times(-0{,}0060)$. Essas resoluções têm 98% de convergência e sua participação na agenda cai de 22,5% (1997–2008) para 18,0% (2009–2016). O exemplo retoma o diagnóstico da Aula 4 de que a agenda muda.
3. **Tratar preditor categórico com k categorias**: continente no recorte de 2016 (África 24, Américas 22, Ásia 18, Europa 30, Oceania 2; sugiro juntar Ásia e Oceania); médias por grupo = intercepto + coeficientes; armadilha das indicadoras; `relevel()`.
4. **Consertar a aplicação com países**: descrever a amostra (Brasil + 95 doadores); tirar `n_pares_validos` dos modelos; dizer a unidade em desvios-padrão uma vez; checar se existe a série original de participação no comércio, que daria unidade substantiva; citar a origem real dos dados.
5. **Aplicar suas regras e recalibrar para cerca de 21 páginas**: tirar os 9 avisos causais; títulos nominais; notação da Aula 5; uma página de laboratório; desacoplar o Rmd do roteiro dos alunos (hoje o Rmd faz `source()` do script 09 e tem `stopifnot()` com números fixos, então qualquer mudança no roteiro quebra ou altera os slides).

## Detalhamento slide a slide

### Slide 1: capa
**Veredicto**: Manter.

### Slide 2: Roteiro da aula
**Narrativa**: A pergunta cita "cobertura da votação", que é artefato de medida. Falta o objetivo da aula (a Aula 5 tem).
**Técnico**: A linha de tempo (65/45 min) está errada para o formato do curso e não pertence ao slide.
**Veredicto**: Revisar.

### Slide 3: Da Aula 5 à Aula 6
**Narrativa**: Bom gancho.
**Técnico**: Troca $\widehat\alpha,\widehat\beta$ por $\widehat\beta_0,\widehat\beta_1,\widehat\beta_2$ sem aviso. Sugestão: $\widehat Y_i=\widehat\alpha+\widehat\beta_1X_{1i}+\widehat\beta_2X_{2i}$, que é a forma do Hansen 2.21 e mantém a Aula 5.
**Veredicto**: Revisar.

### Slide 4: O sentido de manter constante
**Técnico**: A igualdade é útil. Sozinha, repete a forma funcional; precisa ser seguida da decomposição do coeficiente.
**Regras**: alertblock causal.
**Veredicto**: Revisar (tirar o bloco; manter a igualdade).

### Slide 5: Símbolos da regressão múltipla
**Técnico**: "Projeção linear de referência" não é definida; $u_i$ diverge de $\varepsilon$; "desfecho"; população com índice $i$.
**Veredicto**: Reescrever como "BLP com dois preditores": $(\alpha,\beta_1,\beta_2)=\arg\min_{(\widetilde\alpha,\widetilde\beta_1,\widetilde\beta_2)}E[(Y-\widetilde\alpha-\widetilde\beta_1X_1-\widetilde\beta_2X_2)^2]$, $\varepsilon=Y-\alpha-\beta_1X_1-\beta_2X_2$ e $E[\varepsilon]=E[X_1\varepsilon]=E[X_2\varepsilon]=0$.

### Slide 6: Preditor categórico: a referência
**Narrativa**: Aparece antes do critério de MQO e longe da aplicação que o usa.
**Veredicto**: Mover para o bloco de categóricas e fundir com os slides 18–19 numa página sobre categoria e preditor contínuo (retas paralelas).

### Slide 7: Variável omitida: quatro unidades
**Técnico**: Exemplo pequeno e verificável à mão. É determinístico ($R^2=1$ na regressão longa), o que serve para conta.
**Regras**: "Pergunta para a turma".
**Veredicto**: Manter a tabela; usá-la para a decomposição do coeficiente e para o viés de variável omitida.

### Slide 8: A regressão curta e a comparação condicional
**Técnico**: Contas corretas (2,5; 7,5; 5; B–C = 2).
**Veredicto**: Fundir com 7 e 9 depois da fórmula geral.

### Slide 9: A decomposição da omissão
**Técnico**: $5=2+3\times1$ correto. Os rótulos "coeficiente com $Z$" e "associação $X$–$Z$" escondem a direção: o termo é a inclinação de $Z$ sobre $X$. Usar $\gamma_1$, $\beta_1$, $\beta_2$ e $\delta$ da fórmula geral.
**Regras**: alertblock causal.
**Veredicto**: Fundir (ver 8).

### Slide 10: Como o MQO escolhe os coeficientes
**Narrativa**: Deveria abrir o bloco teórico.
**Técnico**: SQR com tils correta. A condição de posto e a forma matricial estão em nota de rodapé; as equações normais não aparecem. "Plano com mais dimensões": com $k$ preditores é um hiperplano.
**Veredicto**: Mover para o início e ampliar.

### Slide 11: A aplicação: países e votos na AGNU
**Técnico**: A amostra (Brasil + 95 doadores) não é descrita. "Painel didático preservado" e `synth_data.rds` são internos.
**Regras**: negação de espantalho causal.
**Veredicto**: Reescrever.

### Slide 12: Conferência dos denominadores
**Narrativa**: Checagem de dados.
**Veredicto**: Mover para o laboratório. O menor denominador (5 pares, Ruanda) pode virar uma frase no slide da aplicação.

### Slide 13: A escala dos preditores
**Técnico**: Conflito de notação com $Z$. Explica a escala de uma variável sem unidade substantiva.
**Veredicto**: Reduzir a uma linha no slide da aplicação.

### Slide 14: Associação bivariada no recorte de 2016
**Visual**: Rótulo do eixo y cortado ("(% dos pares válidos" sem fechar); título interno "Figura 1…" repete o título do slide; alguns pontos com $z>3$ puxam a reta.
**Regras**: legenda com aviso causal e `synth_data.rds`.
**Veredicto**: Revisar (se a aplicação com países ficar).

### Slide 15: Quatro modelos na mesma amostra
**Técnico**: Boa prática: mesma amostra nos quatro modelos. M2 com `n_pares_validos` deve sair.
**Veredicto**: Fundir com 16.

### Slide 16: Coeficiente do comércio com a China
**Técnico**: Valores corretos (0,80; 0,52; 1,29; 1,69).
**Veredicto**: Fundir com 15 numa tabela.

### Slide 17: O coeficiente muda entre especificações
**Visual**: Repete os quatro números do slide 16. A linha liga categorias sem ordem.
**Regras**: título-tese.
**Veredicto**: Cortar. O lugar dele é o slide de viés de variável omitida com dados.

### Slide 18: A categoria geográfica em M4
**Técnico**: Correto (6,70 p.p.). Usa $X$ como vetor de "outros preditores". Mistura 6,70 p.p. no texto e 0,0670 na equação.
**Veredicto**: Fundir com 6 e 19.

### Slide 19: Dois perfis condicionais de M4
**Técnico**: 60,45% e 67,15% corretos; a diferença reproduz o coeficiente. "Previsões da reta": com vários preditores, "valores ajustados".
**Veredicto**: Fundir (ver 18).

### Slide 20: Ajuste descritivo dos modelos
**Técnico**: $R^2$ não foi definido em aula anterior e não está no syllabus da Aula 6. A frase "o $R^2$ do MQO não diminui" é correta para modelos aninhados na mesma amostra.
**Regras**: alertblock causal.
**Veredicto**: Cortar. O professor não usa $R^2$ no curso (instrução de 29/09/2026).

### Slide 21: Limites desta comparação
**Veredicto**: Cortar. Amostra e escala vão para o slide da aplicação; o resto é fala.

### Slides 22–24: Laboratório
**Narrativa**: Três slides repetem o script projetado. A pergunta 1 (por que o coeficiente muda de 0,80 para 1,69) só pode ser respondida com a fórmula de viés de variável omitida, que o deck não dá. A pergunta 4 é causal. "Unidade a escrever na resposta" fala com a turma.
**Veredicto**: Fundir em um slide.

### Slide 25: Síntese
**Regras**: item 3 com aviso causal; frase final genérica.
**Veredicto**: Reescrever com três itens: decomposição do coeficiente, fórmula da regressão curta e longa, categoria de referência.

### Slide 26: Leituras e fontes
**Técnico**: Hansen 2.21–2.25 e Shalizi corretos.
**Veredicto**: Revisar: trocar `synth_data.rds` e "manifesto local" pela origem real dos dados.

## Slides faltando

1. BLP com dois preditores (argmin, $\varepsilon$, condições de primeira ordem). Hansen 2.21–2.22.
2. Equações normais na amostra como plug-in; forma matricial; condição de posto.
3. Decomposição do coeficiente: enunciado na população ($\beta_1=E[u_1Y]/E[u_1^2]$, com $u_1$ o erro da projeção de $X_1$ em $X_2$) e na amostra ($\widehat\beta_1=\sum_i\widehat r_iY_i/\sum_i\widehat r_i^2$).
4. Derivação da decomposição em três linhas: $\sum_i\widehat r_i=\sum_i\widehat r_iX_{2i}=0$ (equações normais da regressão auxiliar); $\sum_i\widehat r_i\widehat e_i=0$ ($\widehat r_i$ é combinação linear de $1$, $X_1$, $X_2$); $\sum_i\widehat r_iX_{1i}=\sum_i\widehat r_i^2$.
5. Decomposição no exemplo de quatro unidades: resíduos $(0;\,-0{,}5;\,0{,}5;\,0)$, $\widehat\beta_1=2$.
6. Fórmula da regressão curta e longa com derivação: substituir $Y_i=\widehat\alpha+\widehat\beta_1X_{1i}+\widehat\beta_2X_{2i}+\widehat e_i$ na inclinação curta; o termo em $\widehat e_i$ é zero pelas equações normais da regressão longa; a identidade vale exatamente na amostra.
7. Tabela de sinais ($\beta_2$ × $\delta$).
8. Viés de variável omitida com dados (Brasil–China por resolução, ou comércio e hiato de poder).
9. Categórica com $k$ níveis, médias por grupo, armadilha das indicadoras e `relevel()`.
10. `lm()` com fator e regressão de resíduos (código curto).

## Cortar ou mover para o laboratório

- Cortar: 17 (figura das especificações), 20 ($R^2$), 21 (limites).
- Mover para o laboratório: 12 (conferência dos denominadores), 22–24 (instruções) como um slide.
- Reduzir: 13 (escala) a uma linha.

## Proposta: 21 páginas para 2 horas

| # | Título | Conteúdo | Reaproveita | Min |
|---|---|---|---|---|
| 1 | Capa | — | 1 | — |
| 2 | Roteiro | Pergunta substantiva, quatro blocos, objetivo | 2 | 3 |
| 3 | Da Aula 5 à Aula 6 | $\widehat\alpha+\widehat\beta X$ → $\widehat\alpha+\widehat\beta_1X_1+\widehat\beta_2X_2$; notação ($\alpha$, $\beta_j$, $\varepsilon$, $\widehat e_i$) | 3 | 4 |
| 4 | BLP com dois preditores | argmin com tils; $\varepsilon$; três condições | novo (5) | 8 |
| 5 | Equações normais | população ↔ amostra (plug-in); $\widehat{\boldsymbol\beta}=(\mathbf X^\top\mathbf X)^{-1}\mathbf X^\top\mathbf Y$; posto completo | 10 | 8 |
| 6 | Interpretação condicional | $\widehat Y(x_1+1,x_2)-\widehat Y(x_1,x_2)=\widehat\beta_1$ | 4 | 4 |
| 7 | Decomposição do coeficiente | Hansen 2.23; resíduo de $X_1$ sobre $X_2$ | novo | 8 |
| 8 | Decomposição: derivação | três linhas a partir das equações normais | novo | 8 |
| 9 | Exemplo: quatro unidades | tabela; resíduos $(0;\,-0{,}5;\,0{,}5;\,0)$; $\widehat\beta_1=2$ | 7–8 | 5 |
| 10 | Regressão curta e longa | $\gamma_1=\beta_1+\beta_2\delta$; derivação; $5=2+3\times1$ | 9 | 8 |
| 11 | Sinal da diferença | tabela 2×2 | novo | 4 |
| 12 | Ano e agenda da AGNU | Brasil–China: 0,31 vs. 0,45 p.p./ano; $0{,}31=0{,}45+23{,}2\times(-0{,}0060)$ | novo | 8 |
| 13 | Preditor categórico | $k$ categorias, $k-1$ indicadoras; continente, 96 países (amostra descrita) | 6 | 5 |
| 14 | Médias por grupo | só a categórica: intercepto = média da referência; coeficientes = diferenças | novo | 5 |
| 15 | Indicadoras e colinearidade | armadilha; `NA` no `lm()`; `relevel()` preserva os ajustados | novo | 5 |
| 16 | Categoria e preditor contínuo | retas paralelas; perfis | 6, 18, 19 | 5 |
| 17 | Especificações | M1–M4 do comércio, sem `n_pares_validos`; unidade em DP | 15, 16 | 5 |
| 18 | MQO no R | `lm(y ~ x1 + x2)`, `factor()`, `relevel()`, regressão de resíduos | novo | 3 |
| 19 | Laboratório | quatro tarefas | 22–24 | 2 |
| 20 | Síntese | três itens | 25 | 2 |
| 21 | Referências | Hansen 2.21–2.25; Shalizi cap. 12 e 14.3; fontes reais | 26 | — |

São 21 páginas e cerca de 105 minutos, com 15 minutos de folga para perguntas. Se faltar tempo, cortar 11 (a tabela de sinais cabe no slide 10) e 17 (fica no laboratório).

Números para os slides novos (conferidos):

- Brasil–China, 1.762 resoluções, $Y$ = voto convergente (0/1), $X_1$ = anos desde 1997, $X_2$ = indicadora de conflito palestino: curta +0,309 p.p./ano; longa +0,448; $\widehat\beta_2=23{,}2$ p.p.; $\widehat\delta=-0{,}0060$ por ano; $0{,}448+23{,}2\times(-0{,}0060)=0{,}309$. Convergência: 98,1% nas resoluções sobre o conflito palestino e 75,4% nas demais. A unidade muda em relação à Aula 5 (resolução, e não ano), por isso a inclinação curta é 0,31 e não 0,39.
- Continente, 2016: médias de convergência com a China de 70,0% (África, 24), 65,9% (Américas, 22), 65,1% (Ásia, 18), 50,0% (Europa, 30) e 67,4% (Oceania, 2). Com África como referência: intercepto 70,02; Américas −4,16; Ásia −4,93; Europa −19,99; Oceania −2,58. `relevel()` para Américas altera os coeficientes e preserva os valores ajustados. Requer o pacote `countrycode` (instalado).
- Comércio e hiato de poder, 2016: curta 0,80; longa 1,66; $\widehat\beta_2=5{,}01$; $\widehat\delta=-0{,}171$; $1{,}66+5{,}01\times(-0{,}171)=0{,}80$ (p.p. por DP).
- Decomposição em M4: regredir a taxa no resíduo do comércio sobre os demais preditores dá 1,685, igual ao coeficiente de M4.

## Laboratório (1h30)

**Diagnóstico.** O roteiro tem 262 linhas, é linear e roda em poucos minutos. O aluno executa blocos prontos e lê as saídas. Cabe em 1h30 com folga, e as atividades exercitam pouco do que a aula deveria ensinar. Pontos específicos:

- a pergunta a) pede explicação para a mudança de M1 a M4 sem dar a ferramenta; a pergunta e) é causal;
- `n_pares_validos` como preditor;
- o Rmd dos slides faz `source()` deste script e fixa números em `stopifnot()`;
- `Rplots.pdf` era gerado em execução por `Rscript` (o Codex estava corrigindo às 15:07);
- calcula $R^2$ por modelo (`summary()$r.squared`), que o professor não usa no curso; mistura de `data.table`, `dplyr` e atribuição base.

**Proposta de 90 minutos, em duplas ou trios:**

1. (10 min) Ler o painel, filtrar 2016, conferir 96 países e descrever a amostra.
2. (20 min) Regressão curta e longa com comércio e hiato de poder; regressão auxiliar; conferir a identidade $\widehat\gamma_1=\widehat\beta_1+\widehat\beta_2\widehat\delta$ com os objetos do R.
3. (20 min) Decomposição do coeficiente: resíduo do comércio sobre os demais preditores de M4; `lm()` da taxa no resíduo; comparar com M4; gráfico de variável adicionada.
4. (20 min) Continente: médias por grupo e coeficientes; `relevel()`; incluir todas as indicadoras e ver o `NA`.
5. (20 min) Modelos progressivos M1–M4 sem `n_pares_validos` e um parágrafo de interpretação com unidade.

## Números conferidos

| Item | Valor no deck | Conferido |
|---|---|---|
| Coeficiente do comércio, M1–M4 (p.p. por DP) | 0,80 / 0,52 / 1,29 / 1,69 | sim (0,8027 / 0,5237 / 1,2939 / 1,6852) |
| $R^2$, M1–M4 | 0,004 / 0,104 / 0,250 / 0,398 | sim |
| América Latina em M4 | 6,70 p.p. | sim (6,701) |
| Perfis de M4 | 60,45% e 67,15% | sim (validação do script) |
| Exemplo de quatro unidades | 5 = 2 + 3 × 1 | sim |
| Denominadores | 5 / 112 / 113 | sim (mínimo: Ruanda) |
| Escala das covariáveis na fonte | "não documentada" | média 0 e DP 0,5 exatos em 1.920 país-anos |

## Coordenação com o Codex

- Às 15:07, o Codex principal ainda estava ativo e tinha editado o laboratório. Antes de qualquer edição minha nos arquivos da Aula 6, é preciso confirmar que ele terminou.
- Se a estrutura proposta for adotada, os números dos slides devem ser calculados no próprio Rmd, como na Aula 5, ou no script docente de validação, sem `source()` do roteiro dos alunos.
