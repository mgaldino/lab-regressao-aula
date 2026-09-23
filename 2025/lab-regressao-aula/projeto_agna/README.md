# Projeto aplicado: Brasil e China na Assembleia Geral da ONU

## Pergunta

Em quais temas e períodos Brasil e China votam juntos na Assembleia Geral das Nações Unidas (AGNU)?

O projeto é um estudo descritivo e associacional sobre votações. A análise acompanha a convergência direta dos votos entre Brasil e China e compara seus padrões entre temas, anos e períodos.

## Unidade de análise

Cada linha representa uma resolução com votação nominal entre 2005 e 2012. A variável de resultado `convergente` vale 1 quando os votos do Brasil e da China são iguais e 0 quando são diferentes.

## Organização dos arquivos

- `data/raw/`: cópia preservada da fonte processada do diagnóstico de votos;
- `data/processed/`: base didática enxuta, dicionário e validação;
- `scripts/01_preparar_dados_agna.R`: preparação reprodutível da base;
- `laboratorios/roteiro_laboratorios.md`: sequência das aulas 2–12.

## Execução

A partir de `2025/lab-regressao-aula/`:

```bash
Rscript --vanilla projeto_agna/scripts/01_preparar_dados_agna.R
```

O script preserva o arquivo bruto e cria `data/processed/dados_ensino_agna.csv`, `dicionario_variaveis.csv` e `validacao_base.csv`.

## Proveniência

A fonte foi produzida no diagnóstico independente de votos Brasil–China do projeto `RDD Trade`, usando o pacote `unvotes` e registros públicos da ONU. A cópia original contém URLs e a data de acesso no campo `source`. A fonte bruta foi mantida separada da base didática para que a transformação possa ser auditada.

## Limites substantivos

O período cobre oito anos e reúne votações de temas distintos. As comparações por ano, tema e período serão apresentadas como descrições e associações dos padrões observados.
