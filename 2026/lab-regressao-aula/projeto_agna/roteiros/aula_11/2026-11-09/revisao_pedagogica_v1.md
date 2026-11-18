# Revisão pedagógica da proposta de Aula 11

Revisão própria de 09/11/2026, aplicando `presentation-script-first` e `pedagogy-review` à fala integral e aos apoios previstos. Não houve subagentes, parecer independente, aplicação em sala ou montagem de slides.

## Diagnóstico geral

O roteiro parte da restrição de inclinação comum usada na Aula 10 e a transforma em uma pergunta sobre a dependência das comparações em relação ao primeiro turno. A derivação por substituição do indicador precede a interpretação do produto. Centralizar torna explícita a comparação respondida pelo coeficiente de nível. A covariância reaparece na combinação de coeficientes antes dos intervalos. A generalização contínua e o exemplo causal são breves e usam os instrumentos já apresentados.

## Avaliação por dimensão

| Dimensão | Nota | Evidência e limite |
|---|---|---|
| Narrativa | A | Duas retas sustentam B1-B4; B7 retoma a necessidade de definir a comparação. |
| Notação e formalismo | A- | X, Y e D são introduzidos antes do produto. Alfa zero e alfa dois aparecem só após a centralização. O bloco causal explicita a nova resposta e sua escala 0/100. A notação requer acompanhamento visual, previsto nas etapas. |
| Pacing | B+ | Contagem registrada em tempo_v1.json; 10min30s de ações; 5min30s de interação. Cenário de 90 ppm: cerca de 43 minutos com a folga mínima, deixando cerca de sete minutos adicionais dentro dos 50. Não houve ensaio. |
| Construção dos conceitos | A | Exemplo conhecido, substituição, comparação, centro, covariância e interpretação. A interpretação causal retoma a Aula 4. |
| Participação | A- | Pergunta sobre distâncias, duas expressões, distinção entre hipóteses e identificação. Quatro momentos de discussão, sem contabilizar novamente o laboratório. |

## Ritmo e custo das derivações

- B1: reconhecer a restrição aditiva, sem novo formalismo.
- B2: maior carga algébrica; duas substituições, agrupamento e cancelamento, com 2min30s adicionais de quadro e 1min30s de resposta.
- B3: distribuir Xc+40 e identificar a nova origem; 1min45s adicional.
- B4: expansão da variância, pesos 10 e 100, raiz, IC e distinção de hipóteses; 2min30s adicional e 1min30s de interação.
- B5: generalização por agrupamento e derivada, sem nova aplicação empírica.
- B6: quatro células hipotéticas, dois efeitos e sua diferença; 1min30s de leitura/cálculo e 1min30s de interação.
- B7: correspondência entre a equação e o código, fechamento e laboratório.

Os tempos calculados estão em `tempo_v1.md`. A reserva adicional pode acomodar a escrita lenta e dúvidas; não é motivo para acrescentar tópicos antes da aprovação.

## Lacunas e riscos corrigidos antes da entrega

1. **Mudança silenciosa de amostra:** a fala explicita as 447 zonas RJ-SP e informa que ambos os modelos serão reestimados nessa mesma subamostra. Não atribui à interação a mudança em relação ao coeficiente do ajuste de 27 UFs da Aula 10.
2. **Coeficiente do produto confundido com inclinação ou efeito marginal:** B2 deriva as duas retas, a diferença de inclinações e a diferença de níveis; B3 identifica o valor do moderador a que pertence o coeficiente de nível.
3. **Inferência sem comparação definida:** B4 deriva a variância ponderada e separa a hipótese de paralelismo da igualdade de níveis em um ponto. Os ICs são pontuais, com referência t aproximada para HC3.
4. **Interação automaticamente causal:** B6 distingue duas intervenções de um tratamento com efeito heterogêneo entre estratos. O exemplo eleitoral permanece associacional; o exemplo de mobilização é identificado como hipotético.
5. **Escala incoerente do resultado binário:** a fala define Y(t,v) em 0/100, de modo que suas médias são porcentagens e a interação de três unidades é três p.p. A codificação alternativa 0/1 é indicada apenas como correspondência de escala.

## Conferências substantivas

- A forma m(x,d) é apresentada como função do modelo. Coincidência com a esperança condicional verdadeira depende da especificação; a leitura associacional não pressupõe causalidade.
- O produto mantém linearidade nos coeficientes. A fórmula de R inclui os dois termos individuais; retirar esses termos impõe restrições adicionais e não é apresentado como escolha automática por valor-p.
- A centralização em 40 p.p. mantém valores ajustados, resíduos, inclinações e inferência para a mesma comparação. Essa invariância foi verificada numericamente, inclusive para a matriz HC3.
- 40 e 50 pertencem ao intervalo comum de X observado em RJ e SP. O roteiro requer mostrar a distribuição de X, porque estar no intervalo comum não garante a mesma densidade de dados nos dois grupos.
- A validade de HC3 sob dependência entre zonas não foi presumida. A referência t com 443 graus de liberdade é aproximada, sem transportar a exatidão do modelo normal homocedástico.
- A tabela de quatro médias foi construída para a aula. A distinção causal usa atribuição aleatória, positividade, consistência e ausência de interferência; não é evidência sobre mobilização eleitoral real.

## Instruções para a montagem, depois da aprovação

Derivações de B2, B3 e B4 devem permanecer no percurso principal, em etapas legíveis. Mostrar a função diferença e seus intervalos, além das duas retas. O bloco causal precisa identificar visualmente os números hipotéticos e a escala. Evitar inserir interpretação causal nas captions do exemplo SP-RJ. O número de slides deve resultar desses apoios, sem compressão de equações para cumprir uma quota.

O roteiro está pronto para aprovação. O deck e o laboratório discente ainda dependem do conteúdo aprovado.
