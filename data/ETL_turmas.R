library(dplyr)

# turmas --------------------------------------------------------------------

# Definindo vetores de atributos a serem selecionados

vetor_atributos_turmas <- c(
'NU_ANO_CENSO',
'ID_TURMA',
'QT_MATRICULAS',
'CO_ENTIDADE'
)

# carregando o dataset
turmas_raw <- arrow::read_parquet(
  file = "./data/raw/TURMAS.parquet",
  col_select = vetor_atributos_turmas  
) 

# criando variavel numero médio de alunos por turma
turmas <- turmas_raw %>% 
  group_by(CO_ENTIDADE) %>% 
  summarise(
    #media_alunos_por_turma=mean(QT_MATRICULAS),
    media_alunos_por_turma=round(mean(QT_MATRICULAS)),
    )

# gravando arquivo no disco
save(turmas, file ="./data/tidy/turmas.rda")
