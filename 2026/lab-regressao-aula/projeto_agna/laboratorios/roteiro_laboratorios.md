# Roteiro do laboratório e progressão do projeto

O laboratório é cumulativo: cada etapa acrescenta uma verificação, uma tabela ou figura numerada, o código R separado e uma interpretação substantiva calibrada ao método disponível.

## Aula 2 — descrição do Brasil

**Pergunta:** como se distribui a convergência direta dos votos de Brasil e China na AGNU entre 1997 e 2016, antes e depois de 2009?

**Unidade:** uma votação nominal em que os votos de Brasil e China foram observados.

**Sequência de 80 minutos:**

| Etapa | Tempo | Produto verificável |
|:--|--:|:--|
| Importação e unidade de análise | 10 min | Frase que define cada linha e o denominador |
| Dimensões, ausências, duplicatas e regras lógicas | 15 min | Tabela 1 de validação |
| Frequências de `convergente` | 10 min | Tabela 2 com contagens e percentuais |
| Média, variância e desvio-padrão | 15 min | Tabela 3 de momentos |
| Comparação 1997–2008 e 2009–2016 | 15 min | Figura 1 e diferença em pontos percentuais |
| Covariância, correlação e interpretação | 10 min | Estatísticas e parágrafo com limites |
| Organização da entrega | 5 min | PDF, Rmd e código R separado |

**Regra de interpretação:** a diferença de 3,9 pontos percentuais é descritiva. Ela não identifica efeito de 2009, do comércio com a China ou de qualquer mecanismo específico.

## Próxima progressão associacional

### Etapa 1 — cross-sections por ano

Relacionar maior fluxo ou participação do comércio com a China a maior alinhamento de voto em cortes transversais separados por ano. Cada resultado deve informar o ano, a unidade, o denominador e a escala da medida comercial.

### Etapa 2 — painel de vários países

Acompanhar a associação ao longo de 1997–2016 nas 96 unidades totais da lista autoritativa. A estrutura é Brasil + 95 países do donor pool; não existem 96 donors mais o Brasil no arquivo atual.

### Etapa 3 — comparadores do donor pool

Comparar o Brasil com os países do donor pool nos quais a China não se tornou o principal destino. Essa etapa ainda deve ser apresentada como associacional até que as suposições de identificação sejam formuladas e avaliadas.

### Etapa 4 — extensão futura do paper

Somente em uma etapa posterior, usar o indicador “China = principal destino” como tratamento definido no desenho do paper. Essa passagem exige separar descrição, estimativa associacional e efeito causal, além de documentar timing, comparadores e pressupostos.

## Regras permanentes

1. preservar os arquivos brutos e registrar hashes;
2. manter cálculos em scripts R separados;
3. usar `dplyr::select()` sempre que selecionar colunas;
4. numerar tabelas e figuras e fornecer captions informativas;
5. informar unidade de análise, denominador, ausências e regras lógicas;
6. não usar linguagem causal antes de um desenho de identificação defensável;
7. documentar a escala das variáveis comerciais antes de interpretá-las.
