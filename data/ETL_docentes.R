library(dplyr)

# docentes --------------------------------------------------------------------

# Definindo vetores de atributos a serem selecionados

vetor_atributos_docentes <- c(
  'ano',
  'ID_DOCENTE',
  'NU_IDADE',
  'TP_SEXO',
  'TP_COR_RACA',
  'IN_NECESSIDADE_ESPECIAL',
  'IN_BAIXA_VISAO',
  'IN_CEGUEIRA',
  'IN_DEF_AUDITIVA',
  'IN_DEF_FISICA',
  'IN_DEF_INTELECTUAL',
  'IN_SURDEZ',
  'IN_SURDOCEGUEIRA',
  'IN_DEF_MULTIPLA',
  'IN_AUTISMO',
  'IN_SUPERDOTACAO',
  'TP_ESCOLARIDADE',
  'TP_SITUACAO_CURSO_1',
  'CO_CURSO_1',
  'IN_LICENCIATURA_1',
  'TP_TIPO_IES_1',
  'CO_CURSO_2',
  'IN_LICENCIATURA_2',
  'CO_CURSO_3',
  'IN_LICENCIATURA_3',
  'IN_ESPECIALIZACAO',
  'IN_MESTRADO',
  'IN_DOUTORADO',
  'IN_ESPECIFICO_ANOS_INICIAIS',
  'IN_ESPECIFICO_ANOS_FINAIS',
  'IN_ESPECIFICO_ENS_MEDIO',
  'TP_TIPO_CONTRATACAO',
  'id_escola'
)

# carregando o dataset
docentes_raw <- arrow::read_parquet(
  file = "./data/raw/DOCENTES.parquet",
  col_select = vetor_atributos_docentes  
) 



# gravando arquivo no disco
save(docentes, file ="./data/tidy/docentes.rda")
