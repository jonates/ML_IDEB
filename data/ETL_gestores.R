library(dplyr)

# GESTORES --------------------------------------------------------------------

# Definindo vetores de atributos a serem selecionados

vetor_atributos_gestores <- c(
  'NU_ANO_CENSO',
  'ID_GESTOR',
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
  'IN_ESPECIALIZACAO',
  'IN_MESTRADO',
  'IN_DOUTORADO',
  'IN_ESPECIFICO_GESTAO',
  'TP_CARGO_GESTOR',
  'TP_TIPO_ACESSO_CARGO',
  'TP_TIPO_CONTRATACAO',
  'CO_ENTIDADE'
)

# criar dummy pdc com ao nenos uma das deficiencias
# criar dummy escolaridade superior
# criar dummy cor negro (pretos e pardos)
# tirar dummy
# tirar acesso ao cargo
# tirar tipo de contratacao

# carregando o dataset
gestores_raw <- arrow::read_parquet(
  file = "./data/raw/GESTOR.parquet",
  col_select = vetor_atributos_gestores  
) 

# Selecionando somente os Diretores de Escolas do total de 187740 gestores
diretores <- gestores_raw %>% filter(TP_CARGO_GESTOR==1)

# criando variavel que testa unicidade de diretor por escola
diretores <- diretores %>% 
  group_by(CO_ENTIDADE) %>% 
   mutate(n_diretores_escola=n())

# Verificano distribuicao das escolas por n de diretores
table(diretores$n_diretores_escola)

# Selecionando somente diretores de escolas com somente 1 diretor
diretores <- diretores %>% filter(n_diretores_escola<2)

# carregando os codigos das escolas do dataset escolas
escolas <- idebr::censo_escolar_2019_escolas %>% 
  select(id_escola, TP_SITUACAO_FUNCIONAMENTO)

# selecionando somente as escolas ativas
escolas_ativas <- escolas %>% filter(TP_SITUACAO_FUNCIONAMENTO==1)

#join escolas com gestores
gestores <- dplyr::left_join( 
  x = escolas_ativas,
  y = diretores,
  c("id_escola" = "CO_ENTIDADE")
)

# gravando arquivo no disco
save(gestores, file ="./data/tidy/gestores.rda")
