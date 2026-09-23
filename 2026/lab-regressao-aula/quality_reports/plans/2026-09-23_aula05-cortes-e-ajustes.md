# Plano: Aula 5 — ajustes de conteúdo e cortes para caber em 2 horas

**Status**: DRAFT
**Data**: 2026-09-23
**Arquivo**: `projeto_agna/laboratorios/slides_aula_05_mqo_bivariado.Rmd` (54 páginas hoje)

## Objetivo

1. Aplicar os ajustes pedidos:
   - bloco "Dois problemas";
   - $\operatorname{V}(X)$ no slide do BLP;
   - objetivos no início da aula;
   - título do slide sobre resíduos.
2. Remover **todas** as menções a causalidade e identificação. O professor trata disso oralmente, se necessário.
3. Cortar a aula de 54 para cerca de 35 páginas.

## Ajustes pedidos

- **Objetivos no início**: o antigo "Mapa conceitual" vira o slide "Roteiro: do BLP ao MQO amostral", logo após a capa. Contém o problema, a ponte com a Aula 3 (o plug-in liga o BLP ao MQO), o que o aluno saberá ao final e o roteiro em 5 blocos.
- **"Dois problemas"**: o bloco sai, porque seu único conteúdo era causal.
- **V(X)**: acrescentar $\operatorname{V}(X)\mapsto n^{-1}\sum_i(X_i-\bar X)^2$ no slide "O MQO é o análogo amostral do BLP".
- **Título do resíduo**: "Valor observado, valor ajustado e resíduo não são sinônimos" passa a "Cada observação se decompõe em valor ajustado e resíduo".

## Remoção do conteúdo causal

| Slide atual | Ação |
|---|---|
| Mapa conceitual: "Dois problemas" e "delimitar o que ela não responde" | remover |
| Pergunta AGNA: bloco "Leitura permitida" | remover |
| Estimação e identificação respondem a perguntas diferentes | **cortar slide** |
| Três elementos: "reta associacional" | "reta" |
| Equações normais no exemplo: bloco "Cuidado" | remover |
| Inclinação em unidades: bloco "Erro comum" | remover |
| Pausa recentralizar: item 4 | slide cortado (ver abaixo) |
| Título "Os coeficientes do AGNA têm leitura associacional" | "Os coeficientes do AGNA nas unidades dos dados" |
| A inclinação positiva não identifica efeito causal | **cortar slide** |
| `lm()`: título e trecho "não cria identificação causal" | título "Em R, `lm()` calcula a mesma reta"; o bloco mantém só o escopo (sem valores-p, testes ou `summary()`) |
| Laboratório: "Que conclusão causal ela não permite?" | remover |
| Pausa final: item 4 e "identificação" no objetivo | remover |
| Cinco erros: item 5 | slide cortado (ver abaixo) |
| Síntese: item 4 e frase final "não inventa um contrafactual" | remover |

## Cortes e fusões por tempo

| # | Slide(s) atual(is) | Ação | Justificativa |
|---|---|---|---|
| 5 | Quando o plug-in oferece boas estimativas? | cortar | A definição formal já traz a hipótese de observações independentes e identicamente distribuídas; as condições de regularidade podem ser ditas oralmente |
| 11 | Três elementos vêm antes da reta | cortar | O slide AGNA já define unidade, $Y_i$ e $X_i$ |
| 13+14 | Reta atribui valor ajustado + observado/ajustado/resíduo | fundir | Um slide com $\widehat Y_i$, a decomposição e a diferença entre $\widehat e_i$ e $\varepsilon$ |
| 16+17+18 | Critério SQR + por que o quadrado + argmin | fundir em 1 | SQR, argmin e a equivalência com o EQM; as razões para o quadrado ficam para a fala |
| 19+20+21 | Função objetivo + CPO do intercepto + CPO da inclinação | fundir em 1 | As duas derivadas cabem em um slide |
| 26 | Pausa: leia a fórmula | cortar | A pergunta sobre o sinal já está em "Quatro pares"; a pergunta sobre o fator $1/n$ vai para o slide do BLP |
| 28 | População e amostra não devem ser confundidas | cortar | A distinção entre $\varepsilon$ e $\widehat e_i$ fica no slide fundido 13+14 |
| 30+31 | Exemplo: inclinação + intercepto | fundir | As contas são curtas |
| 32+33 | Tabela de resíduos + equações normais no exemplo | fundir | Checagem de $\sum\widehat e_i=0$ e $\sum X_i\widehat e_i=0$ em duas linhas sob a tabela |
| 34 | Outra reta produz erro quadrático maior | cortar | A figura seguinte já mostra SQR 4,20 vs. 6,00; o rótulo passa a indicar a reta "$1+X$" |
| 39 | Exemplo de centralização em $X=2$ | cortar | A álgebra geral e a aplicação a 1997 bastam |
| 41 | Pausa: recentralizar em 2009 | cortar | O exercício já está no roteiro do laboratório |
| 42+43 | Valores ajustados + extrapolação | fundir | Um slide sobre interpolação e extrapolação |
| 50 | Propriedades dos resíduos | cortar | Repete as equações normais e a checagem $(\bar X,\bar Y)$ |
| 52 | Cinco erros | cortar | Os erros 2–4 entram como um item da síntese |

## Resultado esperado: 35 páginas

1. Capa
2. Roteiro (novo)
3–6. Plug-in: média; variância viesada; viés vs. consistência; definição formal
7. Figura dos 20 anos
8. Pergunta AGNA
9. Quatro pares
10. Reta, valor ajustado e resíduo
11. Figura dos resíduos
12. Critério e argmin
13. CPO
14. Pausa sobre as duas condições
15. Intercepto
16. Centralizar revela a inclinação
17. Fórmula
18. BLP → MQO, com $\operatorname{V}(X)$ e a pergunta sobre o fator $1/n$
19. Tabela 1
20. $\widehat\beta$ e $\widehat\alpha$
21. Tabela 2 + equações normais
22. Figura comparando as retas
23. Unidades da inclinação
24. Intercepto e $X=0$
25. Centralizar
26. AGNA com 1997 como referência
27. Interpolação e extrapolação
28. Figura AGNA
29. Coeficientes AGNA
30. Extremos
31. `lm()`
32. Laboratório
33. Pausa final
34. Síntese
35. Referências

## Verificação

- [ ] Compilar com XeLaTeX (`LC_ALL=pt_BR.UTF-8`).
- [ ] Conferir contagem de páginas e ausência de overflow nas páginas fundidas (render para PNG e inspeção visual).
- [ ] `grep` por causal/identifica/contrafactual/associacional sem ocorrências.
- [ ] Números inalterados: 0,9; 1,4; 4,20; 6,00; 79,3%; 69,6%; +0,39 p.p.
- [ ] Atualizar `output/aula_05/nota_entrega_aula_05.md`.
