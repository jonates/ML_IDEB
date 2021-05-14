library(dplyr)
library(ggplot2)

# carregando dataset turmas -------------------------------------------------
load("./data/tidy/turmas.rda")

# ordenando o dataset de gestores por codigo da escola
turmas <- turmas %>% 
  rename(id_escola = CO_ENTIDADE) %>% 
  arrange(id_escola) %>% 
  mutate(id_escola = as.character(id_escola))

# pegando o dataset de escolas e selecionando somente as ativas
escolas <- idebr::censo_escolar_2019_escolas %>% 
  select(
    "id_escola",
    "TP_DEPENDENCIA",
    'TP_SITUACAO_FUNCIONAMENTO'
  ) %>% 
  arrange(id_escola) %>% 
  mutate(id_escola = as.character(id_escola)) %>% 
  filter(TP_SITUACAO_FUNCIONAMENTO==1)

# juntando turmas com escolas
turmas <- dplyr::left_join(escolas, turmas, "id_escola")

#verificando districao das escolas por tipo de dependencia
table(turmas$TP_DEPENDENCIA)

# retirando escolas privadas
turmas <- turmas %>% filter(TP_DEPENDENCIA != 4)

#verificando se as escolas privadas sairam
table(turmas$TP_DEPENDENCIA)

# montando o dataset ideb 2019 por escola ------------------------------------------

# carregando o dataset de ideb
ideb_medio <-idebr::ideb_ensino_medio_escolas %>% 
  filter(ano==2019) %>% 
  mutate(etapa = '3') %>%
  mutate(
    cat_matematica = ifelse(nota_matematica<325,0,1),
    cat_portugues = ifelse(nota_portugues<300,0,1)
  )

ideb_sf <- idebr::ideb_fundamental_finais_escolas %>% 
  filter(ano==2019) %>% 
  mutate(etapa = '2') %>%
  mutate(
    cat_matematica = ifelse(nota_matematica<300,0,1),
    cat_portugues = ifelse(nota_portugues<275,0,1)
  )

ideb_si <- idebr::ideb_fundamental_iniciais_escolas %>% 
  filter(ano==2019) %>% 
  mutate(etapa = '1') %>%
  mutate(
    cat_matematica = ifelse(nota_matematica<225,0,1),
    cat_portugues = ifelse(nota_portugues<200,0,1)
  )

# juntando os dataset do ideb 2019
ideb_2019 <- ideb_si %>% 
  bind_rows(ideb_sf) %>% 
  bind_rows(ideb_medio) %>% 
  arrange(id_escola)

# juntando a base do ideb com informacoes dos docentes da escola --------------

ideb_turmas <- dplyr::left_join(ideb_2019,turmas,"id_escola")

# verificando as dependencias nas tabelas abaixo
# Nem todas as escolas tem informacoes de ideb
# Tem escolas que oferecem mais de uma etapa de ensino
table(turmas$TP_DEPENDENCIA)
table(ideb_turmas$TP_DEPENDENCIA)

# removendo objetos desnecessarios
rm(turmas, escolas, ideb_2019,ideb_medio,ideb_sf,ideb_si)

# transformando notas de matematica  e portugues em numerica
ideb_turmas <- ideb_turmas %>% 
  mutate(
    nota_matematica = as.numeric(stringr::str_replace(nota_matematica,",",".")),
    nota_portugues = as.numeric(stringr::str_replace(nota_portugues,",",".")),
    nota_media = as.numeric(stringr::str_replace(nota_media,",",".")),
    cat_tamanho_turma = ifelse(media_alunos_por_turma<=22,0,1)
  )

# retirando os na's
ideb_turmas <- ideb_turmas %>% filter(!is.na(media_alunos_por_turma))

#tabela etapa de ensino x target

gmodels::CrossTable(
  ideb_turmas$etapa,
  ideb_turmas$cat_matematica,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("SI (1) SF (2) Medio (3)", "Aprendizado de matemática adequado"),
  format = "SPSS"
)

gmodels::CrossTable(
  ideb_turmas$etapa,
  ideb_turmas$cat_portugues,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("SI (1) SF (2) Medio (3)", "Aprendizado de portugues adequado"),
  format = "SPSS"
) 

#tabela rede x target

gmodels::CrossTable(
  ideb_turmas$rede,
  ideb_turmas$cat_matematica,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("rede de ensino", "Aprendizado de matemática adequado"),
  format = "SPSS"
)

gmodels::CrossTable(
  ideb_turmas$rede,
  ideb_turmas$cat_portugues,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("rede de ensino", "Aprendizado de portugues adequado"),
  format = "SPSS"
)  

#tabela UF x target

gmodels::CrossTable(
  ideb_turmas$sigla_uf,
  ideb_turmas$cat_matematica,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("etapa de ensino", "Aprendizado de matemática adequado"),
  format = "SPSS"
)

gmodels::CrossTable(
  ideb_turmas$sigla_uf,
  ideb_turmas$cat_portugues,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("etapa de ensino", "Aprendizado de portugues adequado"),
  format = "SPSS"
) 


#tabela tamanho de turma x target

gmodels::CrossTable(
  ideb_turmas$cat_tamanho_turma,
  ideb_turmas$cat_matematica,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("turma <= 22", "Aprendizado de matemática adequado"),
  format = "SPSS"
)

gmodels::CrossTable(
  ideb_turmas$cat_tamanho_turma,
  ideb_turmas$cat_portugues,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("turma <= 22", "Aprendizado de portugues adequado"),
  format = "SPSS"
)



# Descritiva do numero medio de alunos por turma
summary(ideb_turmas$media_alunos_por_turma)
boxplot(ideb_turmas$media_alunos_por_turma)
plot(
  x = ideb_turmas$media_alunos_por_turma, 
  y = ideb_turmas$nota_matematica,
  xlim = c(0,50)
)

# modelo linear entre media de alunos e notas
lm(ideb_turmas$nota_matematica ~ ideb_turmas$media_alunos_por_turma)
lm(ideb_turmas$nota_portugues ~ ideb_turmas$media_alunos_por_turma)

# media de alunos por turma de acordo com o desempenho
psych::describeBy(ideb_turmas$media_alunos_por_turma,ideb_turmas$cat_matematica)
psych::describeBy(ideb_turmas$media_alunos_por_turma,ideb_turmas$cat_portugues)

# teste para diferenca de media entre desempenhos
t.test(
  ideb_turmas %>% filter(cat_matematica==0) %>% pull(media_alunos_por_turma),
  ideb_turmas %>% filter(cat_matematica==1) %>% pull(media_alunos_por_turma)
)


# nota de matematica por IN_MESTRADO --------------------------
psych::describeBy(xlim = 
  x = ideb_gestores$nota_matematica,
  group =  ideb_gestores$IN_MESTRADO
)

ggplot(
  data = ideb_gestores %>% filter(!is.na(IN_MESTRADO)), 
  aes(
    x = nota_matematica,
    y = ..density..,
    fill = IN_MESTRADO
  )
) + 
geom_histogram(alpha = 0.8,)

t.test(
  ideb_gestores %>% filter(IN_MESTRADO==0) %>% pull(nota_matematica),
  ideb_gestores %>% filter(IN_MESTRADO==1) %>% pull(nota_matematica)
)
# nota de portugues por IN_MESTRADO --------------------------
psych::describeBy(
  x = ideb_gestores$nota_portugues,
  group =  ideb_gestores$IN_MESTRADO
)

ggplot(
  data = ideb_gestores %>% filter(!is.na(IN_MESTRADO)), 
  aes(
    x = nota_portugues,
    y = ..density..,
    fill = IN_MESTRADO
  )
) + 
  geom_histogram(alpha = 0.8,)

# nota de media por IN_MESTRADO --------------------------
psych::describeBy(
  x = ideb_gestores$nota_media,
  group =  ideb_gestores$IN_MESTRADO
)

ggplot(
  data = ideb_gestores %>% filter(!is.na(IN_MESTRADO)), 
  aes(
    x = nota_media,
    y = ..density..,
    fill = IN_MESTRADO
  )
) + 
  geom_histogram(alpha = 0.8,)

# nota de matematica por IN_DOUTORADO --------------------------
psych::describeBy(
  x = ideb_gestores$nota_matematica,
  group =  ideb_gestores$IN_DOUTORADO
)

ggplot(
  data = ideb_gestores %>% filter(!is.na(IN_DOUTORADO)), 
  aes(
    x = nota_matematica,
    y = ..density..,
    fill = IN_DOUTORADO
  )
) + 
  geom_histogram(alpha = 0.8,)

# nota de portugues por IN_MESTRADO --------------------------
psych::describeBy(
  x = ideb_gestores$nota_portugues,
  group =  ideb_gestores$IN_DOUTORADO
)

ggplot(
  data = ideb_gestores %>% filter(!is.na(IN_DOUTORADO)), 
  aes(
    x = nota_portugues,
    y = ..density..,
    fill = IN_MESTRADO
  )
) + 
  geom_histogram(alpha = 0.8,)

# nota de media por IN_MESTRADO --------------------------
psych::describeBy(
  x = ideb_gestores$nota_media,
  group =  ideb_gestores$IN_DOUTORADO
)

ggplot(
  data = ideb_gestores %>% filter(!is.na(IN_DOUTORADO)), 
  aes(
    x = nota_media,
    y = ..density..,
    fill = IN_MESTRADO
  )
) + 
  geom_histogram(alpha = 0.8,)
