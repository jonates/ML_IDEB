library(dplyr)
library(ggplot2)

# carregando dataset turmas -------------------------------------------------
load("./data/tidy/turmas.rda")

# ordenando o dataset de gestores por codigo da escola
turmas <- turmas %>% 
  rename(id_escola = CO_ENTIDADE) %>% 
  arrange(id_escola) %>% 
  mutate(id_escola = as.character(id_escola))

# montando o dataset ideb 2019 por escola ------------------------------------------

# carregando o dataset de ideb
ideb_medio <-idebr::ideb_ensino_medio_escolas %>% filter(ano==2019)
ideb_sf <- idebr::ideb_fundamental_finais_escolas %>% filter(ano==2019)
ideb_si <- idebr::ideb_fundamental_iniciais_escolas %>% filter(ano==2019)

# juntando os dataset do ideb 2019
ideb_2019 <- ideb_si %>% 
  bind_rows(ideb_sf) %>% 
  bind_rows(ideb_medio) %>% 
  arrange(id_escola)

# juntando a base do ideb com informacoes dos docentes da escola --------------

ideb_turmas <- dplyr::left_join(ideb_2019,turmas,"id_escola")

# removendo objetos desnecessarios
rm(turmas,ideb_2019,ideb_medio,ideb_sf,ideb_si)

# transformando notas de matematica  e portugues em numerica
ideb_turmas <- ideb_turmas %>% 
  mutate(
    nota_matematica = as.numeric(stringr::str_replace(nota_matematica,",",".")),
    nota_portugues = as.numeric(stringr::str_replace(nota_portugues,",",".")),
    nota_media = as.numeric(stringr::str_replace(nota_media,",","."))
  )

# retirando os na's
ideb_turmas <- ideb_turmas %>% filter(!is.na(media_alunos_por_turma))
  
# Descritiva do numero medio de alunos por turma
summary(ideb_turmas$media_alunos_por_turma)
boxplot(ideb_turmas$media_alunos_por_turma)
plot(
  x = ideb_turmas$media_alunos_por_turma, 
  y = ideb_turmas$nota_matematica,
  xlim = c(0,50)
)

lm(ideb_turmas$nota_matematica ~ ideb_turmas$media_alunos_por_turma)

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
