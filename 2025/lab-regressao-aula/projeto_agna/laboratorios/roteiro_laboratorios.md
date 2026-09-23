# Roteiro dos laboratórios — aulas 2 a 12

O laboratório é cumulativo. Em cada aula, as duplas acrescentam uma figura ou tabela, um trecho de código e um parágrafo de interpretação ao mesmo caderno de análise.

## Pergunta comum

Em quais temas e períodos Brasil e China votam juntos na AGNU?

## Sequência

| Aula | Tópico teórico | Tarefa aplicada | Produto mínimo |
|---|---|---|---|
| 2 | Distribuições, momentos, variância e correlação | Importar a base, verificar a unidade de análise e descrever `convergente`, `ano`, `tema` e `eleitorado_valido`. | Tabela de frequências e uma figura descritiva. |
| 3 | Esperança condicional e BLP | Calcular a convergência média por tema e por período. | Tabela de médias condicionais e duas previsões simples. |
| 4 | Causalidade e identificação | Mapear explicações alternativas para a diferença entre períodos e as condições necessárias para uma interpretação causal. | Um diagrama simples e um parágrafo de limites. |
| 5 | MQO bivariado | Estimar um modelo linear simples de `convergente` em função de `ano`. | Linha ajustada e interpretação do coeficiente. |
| 6 | Regressão múltipla | Estimar modelos por blocos: características da votação; participação do comércio com China e EUA; hiato de poder em relação aos EUA; e controles como renda per capita e câmbio. | Tabela de especificações progressivas e interpretação da mudança nos coeficientes. |
| 7 | Propriedades do MQO | Simular amostras repetidas a partir de um processo gerador conhecido e contrastá-las com a reamostragem bootstrap dos dados observados. | Gráfico comparando a distribuição Monte Carlo e a distribuição bootstrap dos estimadores. |
| 8 | Resíduos e diagnóstico | Examinar resíduos, valores ajustados, alavancagem e observações influentes. | Painel de diagnósticos e comentário. |
| 9 | Erros-padrão | Comparar erros-padrão convencionais, HC3 e bootstrap, com reamostragem compatível com a unidade de análise. | Tabela com as três versões e nota metodológica. |
| 10 | Intervalos e testes | Construir intervalos de confiança e testar hipóteses substantivas modestas. | Uma conclusão com estimativa e incerteza. |
| 11 | Interações | Estimar `tema × periodo_2009` como heterogeneidade descritiva. | Figura de previsões por tema e período, com interpretação descritiva. |
| 12 | GLM | Estimar regressão logística para `convergente` e comparar probabilidades preditas com o MQO. | Figura de probabilidades e síntese do projeto. |

## Regras de interpretação

1. Na fonte bruta, `vote_brazil` e `vote_china` são os componentes usados para construir a variável resposta principal de convergência.
2. `periodo_2009` organiza as resoluções em dois períodos para comparação descritiva.
3. Resultados devem informar a unidade de análise, o denominador e as ausências de `voto_importante`.
4. As conclusões caracterizarão as associações observadas entre comércio e convergência.

## Nota para a preparação da Aula 6

A base da Aula 6 deverá ser ampliada para incluir outros países e usará como variável resposta a convergência direta de votos com a China. Isso permite explorar os preditores substantivos do paper -- participação do comércio com China e EUA, hiato de poder em relação aos EUA, distância aos EUA, renda per capita, câmbio e outros controles -- com variação suficiente. Os modelos serão apresentados como associações e construídos por blocos, para que os alunos acompanhem como a inclusão de cada conjunto de preditores altera os coeficientes e a incerteza.
