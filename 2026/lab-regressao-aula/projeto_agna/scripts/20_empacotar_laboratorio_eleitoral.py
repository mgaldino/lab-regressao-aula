#!/usr/bin/env python3
"""Registra integridade e empacota as entradas discentes da Aula 7.

Executar após os scripts 17, 18 e 19. Não acessa a rede. Biblioteca padrão.
O pacote discente tem os CSVs preparados, scripts 18/14 e projeto RStudio.
"""
import csv
import datetime as dt
import hashlib
import json
from pathlib import Path
import shutil
import zipfile

ROOT = Path(__file__).resolve().parents[2]
PROJ = ROOT / 'projeto_agna'
DATA = PROJ / 'data' / 'eleicoes_2022_2026'
DOCS = DATA / 'docs'
OUT = PROJ / 'output' / 'aula_07' / 'laboratorio_eleitoral'
ORIGEM = Path('/Users/manoelgaldino/Documents/DCP/Papers/electoralFraud')


def sha(path):
    h = hashlib.sha256()
    with path.open('rb') as f:
        for b in iter(lambda: f.read(1024 * 1024), b''):
            h.update(b)
    return h.hexdigest()


def ler_csv(path):
    with path.open(encoding='utf-8', newline='') as f:
        return list(csv.DictReader(f))


def main():
    docente = (OUT / 'validacao_laboratorio_eleitoral.txt').read_text()
    assert docente.startswith('VALIDACAO_LAB_ELEITORAL_OK')
    # Preserva o script de transformação anterior para documentar o snapshot.
    anterior = DOCS / 'origem_06b_section_votes.R'
    if not anterior.exists():
        shutil.copyfile(ORIGEM / 'R' / 'eleicao2026' / '06b_section_votes.R', anterior)
    treino = ler_csv(DATA / 'processed' / 'presidente_2022_zonas_com_exterior.csv')
    futuro = ler_csv(DATA / 'processed' / 'presidente_2026_t1_zonas_com_exterior.csv')
    n_treino = f'{len(treino):,}'.replace(',', '.')
    n_futuro = f'{len(futuro):,}'.replace(',', '.')
    mudancas = ler_csv(DOCS / 'zonas_com_chave_em_apenas_um_ano_com_exterior.csv')
    manifestos = sorted((DATA / 'raw').glob('*/manifesto.json'))
    acesso = json.loads(manifestos[-1].read_text())[0]['acesso_utc']
    novos = sum(r['em_2022'] == 'FALSE' for r in mudancas)
    ausentes = sum(r['em_2026'] == 'FALSE' for r in mudancas)
    log = f'''# Relatório de coleta e preparação

Gerado automaticamente em {dt.datetime.now(dt.timezone.utc).isoformat()}.
Última consulta oficial registrada: {acesso}.

## Cobertura obtida

- 2022: {n_treino} zonas nas 27 UFs e Exterior (ZZ), com os dois turnos. Nenhuma chave perdida no cruzamento entre turnos.
- 2026: {n_futuro} zonas nas 27 UFs e Exterior (ZZ), com o primeiro turno. Todas recebem previsão.
- Entre anos: {novos} chaves aparecem apenas em 2026 e {ausentes} apenas em 2022.
- ZZ é uma zona agregada com uma linha em cada ano. Códigos iguais entre anos não asseguram território igual.
- CSVs em UTF-8. Votos são contagens não negativas, denominadores positivos e porcentagens em 0 a 100.
- Chaves UF + zona são únicas. As entradas do laboratório não têm valores ausentes.

## Diferença encontrada e resolvida em 2026

O snapshot por BU continha 119.296.853 votos válidos, incluindo exterior. O JSON nacional oficial registra 119.300.788. A diferença é 3.935 votos válidos. As listas oficiais identificam 3 seções ausentes em Betim (MG, zona 319) e 12 em Carapicuíba (SP, zona 388). São as 15 seções sem BU no Brasil registradas no recibo de cobertura.

Em cada município, todas as seções ausentes pertencem a uma única zona. Subtrai-se a soma dos BUs disponíveis do total municipal oficial e acrescenta-se a diferença à soma dessa zona. Essa identificação torna a alocação única. Não se estimam votos por seção nem se faz rateio entre zonas. O complemento de Betim é 631 votos válidos e o de Carapicuíba é 3.304. Brancos e nulos também são completados pelas diferenças oficiais.

Os 12 totais de candidatos e os votos válidos, após o complemento e incluindo ZZ, igualam a divulgação oficial nacional. A zona permanece a unidade do arquivo processado. A coluna de seções completadas e a tabela de complementos registram a transformação. Os Parquets originais não foram alterados.

O recibo registra ainda 41 seções sem BU no exterior. Os complementos brasileiros esgotam a diferença de votos válidos em relação ao total oficial nacional. A soma de votos válidos não tem diferença residual. Isso não estabelece a completude de todas as informações de eleitorado por seção.

## Fontes e filtros

2022: extratos tratados por seção de votação nominal e detalhe da apuração do TSE. 2026: extrato tratado de BUs da API do TSE, complementado com totais municipais oficiais. Preserva-se a atribuição da origem dos extratos ao projeto local electoralFraud. A reprodução desta preparação começa nos três Parquets preservados no curso. Os manifestos das consultas JSON registram URL, acesso UTC, tamanho e SHA-256. Os arquivos ZIP originais de todos os cargos não foram copiados para este pacote.

Filtros: cargo Presidente, dois turnos em 2022 e primeiro turno em 2026, 27 UFs e Exterior (ZZ). Primeiro somam-se contagens por categoria geográfica + zona. Depois calculam-se porcentagens sobre todos os votos válidos de cada turno. A porcentagem do primeiro turno não é renormalizada entre os dois candidatos finalistas.

## Verificações executadas

{docente}

A duração de 75 minutos é planejada. A adequação da relação de 2022 para prever o segundo turno de 2026 não foi validada fora do treino. O bootstrap condiciona no número de zonas por categoria e nas entradas de 2026; ZZ, com uma linha, não varia internamente. A agregação supõe pesos relativos de votos válidos iguais aos do primeiro turno de 2026.
'''
    (DOCS / 'COLLECTION_LOG.md').write_text(log, encoding='utf-8')
    files = sorted(p for p in DATA.rglob('*') if p.is_file() and
                   p.name not in ('manifesto_arquivos.csv', 'checksums.sha256'))
    (DATA / 'checksums.sha256').write_text(''.join(
        f'{sha(p)}  {p.relative_to(DATA).as_posix()}\n' for p in files), encoding='utf-8')
    with (DOCS / 'manifesto_arquivos.csv').open('w', encoding='utf-8', newline='') as f:
        w = csv.writer(f, lineterminator='\n')
        w.writerow(['arquivo', 'bytes', 'sha256'])
        for p in files:
            w.writerow([p.relative_to(DATA).as_posix(), p.stat().st_size, sha(p)])
    student_readme = '''# Aula 7: regressão e previsão eleitoral por zona

1. Extraia a pasta inteira e abra lab-regressao-aula-2026.Rproj no RStudio.
2. Se necessário, instale os pacotes: install.packages(c("dplyr", "ggplot2", "here", "scales")).
3. Abra projeto_agna/scripts/18_laboratorio_aula7_previsao_eleitoral.R e execute por blocos.
4. Registre as respostas às seis perguntas e a interpretação final. Duração prevista: 75 minutos.

Os dois CSVs já contêm os dados públicos do TSE preparados por UF/Exterior + zona. Não é necessário baixar arquivos em sala. Há 2.637 zonas em 2022 e 2.641 em 2026, incluindo uma zona agregada do Exterior (ZZ) em cada ano. A porcentagem tem como denominador todos os votos válidos do respectivo turno. O ajuste usa Lula nos dois turnos de 2022, disputados contra Jair Bolsonaro. A aplicação prevê Lula versus Flávio Bolsonaro no segundo turno de 2026.

O modelo com categorias geográficas estima a porcentagem de Lula. A do adversário é seu complemento para 100. A agregação nacional usa votos válidos de 2026 T1 como pesos e depende da estabilidade desses pesos relativos entre turnos. Aplicar o modelo a outro ano também depende da estabilidade da relação entre turnos. O bootstrap é um exercício de sensibilidade do ajuste à reamostragem e não inclui toda a incerteza futura; ZZ, com uma linha, permanece fixa.

O script exporta resultados e dois gráficos PDF em projeto_agna/output/aula_07/laboratorio_eleitoral/. O script 14 é uma atividade complementar de Monte Carlo com dados sintéticos e não integra os 75 minutos da atividade eleitoral.

Proveniência e variáveis: veja o diretório projeto_agna/data/eleicoes_2022_2026/docs. O pacote discente tem somente os CSVs preparados. Os snapshots e os scripts de coleta, preparação e validação docente permanecem no repositório do curso.
'''
    entradas = {
        'lab-regressao-aula-2026.Rproj': ROOT / 'lab-regressao-aula-2026.Rproj',
        'projeto_agna/scripts/18_laboratorio_aula7_previsao_eleitoral.R': PROJ / 'scripts' / '18_laboratorio_aula7_previsao_eleitoral.R',
        'projeto_agna/scripts/14_laboratorio_aula7_monte_carlo_bootstrap.R': PROJ / 'scripts' / '14_laboratorio_aula7_monte_carlo_bootstrap.R',
    }
    for f in ('presidente_2022_zonas_com_exterior.csv',
              'presidente_2026_t1_zonas_com_exterior.csv'):
        entradas[f'projeto_agna/data/eleicoes_2022_2026/processed/{f}'] = DATA / 'processed' / f
    for f in ('SOURCES.yaml', 'DATA_DICTIONARY.md', 'COLLECTION_LOG.md'):
        entradas[f'projeto_agna/data/eleicoes_2022_2026/docs/{f}'] = DOCS / f
    extra = {'README.md': student_readme.encode('utf-8')}
    contents = {name: path.read_bytes() for name, path in entradas.items()}
    contents.update(extra)
    contents['checksums.sha256'] = ''.join(
        f'{hashlib.sha256(body).hexdigest()}  {name}\n' for name, body in sorted(contents.items())
    ).encode('utf-8')
    destino = ROOT / 'output' / 'laboratorio_aula_07_eleicoes_2026_alunos.zip'
    destino.parent.mkdir(parents=True, exist_ok=True)
    prefix = 'laboratorio_aula_07_eleicoes_2026/'
    with zipfile.ZipFile(destino, 'w', compression=zipfile.ZIP_DEFLATED) as z:
        for name, body in contents.items():
            z.writestr(prefix + name, body)
    with zipfile.ZipFile(destino) as z:
        assert z.testzip() is None
        for name, body in contents.items():
            assert z.read(prefix + name) == body
    print(f'PACOTE_LAB_ELEITORAL_OK | {destino} | {destino.stat().st_size} bytes')


if __name__ == '__main__':
    main()
