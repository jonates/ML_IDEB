library(dplyr)

# carregando dataset escolas -------------------------------------------------
escolas_raw <- idebr::censo_escolar_2019_escolas

# mantendo somente os atributos discutidos previamente 
escolas <- escolas_raw %>% 
  select("ano",
         "id_escola",
         "TP_SITUACAO_FUNCIONAMENTO",
         "cod_regiao",
         "cod_UF",
         "cod_municipio",
         "TP_DEPENDENCIA",
         "TP_LOCALIZACAO",
         "TP_LOCALIZACAO_DIFERENCIADA",
         "IN_VINCULO_SECRETARIA_EDUCACAO",
         "IN_VINCULO_SEGURANCA_PUBLICA",
         "IN_VINCULO_SECRETARIA_SAUDE",
         "IN_VINCULO_OUTRO_ORGAO",
         "IN_LOCAL_FUNC_PREDIO_ESCOLAR",
         "TP_OCUPACAO_PREDIO_ESCOLAR",
         "IN_AGUA_POTAVEL",
         "IN_AGUA_REDE_PUBLICA",
         "IN_ENERGIA_REDE_PUBLICA",
         "IN_ESGOTO_REDE_PUBLICA",
         "IN_ESGOTO_FOSSA_SEPTICA",
         "IN_ESGOTO_INEXISTENTE",
         "IN_LIXO_SERVICO_COLETA",
         "IN_AUDITORIO",
         "IN_BANHEIRO",
         "IN_BANHEIRO_PNE",
         "IN_BIBLIOTECA",
         "IN_BIBLIOTECA_SALA_LEITURA",
         "IN_COZINHA",
         "IN_LABORATORIO_CIENCIAS",
         "IN_LABORATORIO_INFORMATICA",
         "IN_QUADRA_ESPORTES",
         "IN_REFEITORIO",
         "IN_SALA_ATELIE_ARTES",
         "IN_SALA_MUSICA_CORAL",
         "IN_SALA_ESTUDIO_DANCA",
         "IN_SALA_MULTIUSO",
         "IN_SALA_LEITURA",
         "IN_SALA_PROFESSOR",
         "IN_SECRETARIA",
         "IN_ACESSIBILIDADE_INEXISTENTE",
         "QT_SALAS_UTILIZADAS",
         "QT_SALAS_UTILIZA_CLIMATIZADAS",
         "IN_EQUIP_PARABOLICA",
         "IN_COMPUTADOR",
         "IN_EQUIP_TV",
         "IN_EQUIP_LOUSA_DIGITAL",
         "IN_EQUIP_MULTIMIDIA",
         "IN_DESKTOP_ALUNO",
         "IN_COMP_PORTATIL_ALUNO",
         "IN_TABLET_ALUNO",
         "IN_INTERNET_ALUNOS",
         "IN_INTERNET_APRENDIZAGEM",
         "IN_BANDA_LARGA",
         "QT_PROF_ADMINISTRATIVOS",
         "QT_PROF_SERVICOS_GERAIS",
         "QT_PROF_BIBLIOTECARIO",
         "QT_PROF_SAUDE",
         "QT_PROF_COORDENADOR",
         "QT_PROF_FONAUDIOLOGO",
         "QT_PROF_NUTRICIONISTA",
         "QT_PROF_PSICOLOGO",
         "QT_PROF_ALIMENTACAO",
         "QT_PROF_PEDAGOGIA",
         "QT_PROF_SECRETARIO",
         "QT_PROF_SEGURANCA",
         "QT_PROF_MONITORES",
         "IN_ALIMENTACAO",
         "IN_SERIE_ANO",
         "IN_MATERIAL_PED_MULTIMIDIA",
         "IN_MATERIAL_PED_CIENTIFICO",
         "IN_MATERIAL_PED_MUSICAL",
         "IN_MATERIAL_PED_JOGOS",
         "IN_MATERIAL_PED_ARTISTICAS",
         "IN_MATERIAL_PED_DESPORTIVA",
         "IN_MATERIAL_PED_INDIGENA",
         "IN_MATERIAL_PED_ETNICO",
         "IN_MATERIAL_PED_CAMPO",
         "IN_EDUCACAO_INDIGENA",
         "IN_EXAME_SELECAO",
         'IN_RESERVA_PPI',
         'IN_RESERVA_RENDA',
         'IN_RESERVA_PUBLICA',
         'IN_RESERVA_PCD',
         'IN_RESERVA_OUTROS',
         'IN_ORGAO_ASS_PAIS',
         'IN_ORGAO_ASS_PAIS_MESTRES',
         'IN_ORGAO_CONSELHO_ESCOLAR',
         'IN_ORGAO_GREMIO_ESTUDANTIL',
         'TP_PROPOSTA_PEDAGOGICA',
         'TP_ATIVIDADE_COMPLEMENTAR',
         ) %>% 
  hablar::convert(hablar::chr(id_escola)) %>% 
  arrange(id_escola)

#filtrar escolas ativas
escolas <- escolas %>% 
  filter(TP_SITUACAO_FUNCIONAMENTO==1)

# retirando escolas privadas
escolas <- escolas %>% 
  filter(TP_DEPENDENCIA != 4)

#carregando dataset com informacoes de regioes do Brasil
info_IBGE_regioes <- read.csv2("https://raw.githubusercontent.com/jonates/opendata/master/codigos_IBGE_UF_Regioes/codigos_IBGE_UF_Regioes.csv")
View(info_IBGE_regioes)

# carregando o dataset de ideb
ideb_medio <-idebr::ideb_ensino_medio_escolas %>% 
  filter(ano==2019) %>% 
  mutate(etapa = '3') %>%
  mutate(
    cat_matematica = ifelse(nota_matematica<325,0,1),
    cat_portugues = ifelse(nota_portugues<300,0,1)
  ) %>% 
  select(id_escola,etapa,cat_matematica,cat_portugues)

ideb_sf <- idebr::ideb_fundamental_finais_escolas %>% 
  filter(ano==2019) %>% 
  mutate(etapa = '2') %>%
  mutate(
    cat_matematica = ifelse(nota_matematica<300,0,1),
    cat_portugues = ifelse(nota_portugues<275,0,1)
  ) %>% 
  select(id_escola,etapa,cat_matematica,cat_portugues)

ideb_si <- idebr::ideb_fundamental_iniciais_escolas %>% 
  filter(ano==2019) %>% 
  mutate(etapa = '1') %>%
  mutate(
    cat_matematica = ifelse(nota_matematica<225,0,1),
    cat_portugues = ifelse(nota_portugues<200,0,1)
  ) %>% 
  select(id_escola,etapa,cat_matematica,cat_portugues)

# juntando a base do ideb com informacoes das escolas --------------
ideb_escolas_si <- dplyr::left_join(ideb_si, escolas, "id_escola") 
ideb_escolas_sf <- dplyr::left_join(ideb_sf, escolas, "id_escola") 
ideb_escolas_medio <- dplyr::left_join(ideb_medio, escolas, "id_escola") 
  
# juntando os dataset do ideb 2019
ideb_escolas <- ideb_escolas_si %>% 
  bind_rows(ideb_escolas_sf) %>% 
  bind_rows(ideb_escolas_medio) %>% 
  arrange(id_escola)

# verificando as dependencias nas tabelas abaixo
# Nem todas as escolas tem informacoes de ideb
# Tem escolas que oferecem mais de uma etapa de ensino
janitor::tabyl(ideb_escolas$TP_DEPENDENCIA)
janitor::tabyl(escolas$TP_DEPENDENCIA)

# removendo objetos desnecessarios
rm(escolas_raw, escolas,ideb_medio,ideb_sf,ideb_si,info_IBGE_regioes,ideb_escolas_si,ideb_escolas_sf,ideb_escolas_medio)

# analise ---------------------------------------------------------------------


#Verificando distribuicao de frequencias dos atributos
janitor::tabyl(ideb_escolas$etapa)
janitor::tabyl(ideb_escolas$cod_regiao)
janitor::tabyl(ideb_escolas$TP_SITUACAO_FUNCIONAMENTO)
janitor::tabyl(ideb_escolas$TP_DEPENDENCIA)
janitor::tabyl(ideb_escolas$TP_LOCALIZACAO)
janitor::tabyl(ideb_escolas$IN_BIBLIOTECA)
janitor::tabyl(ideb_escolas$IN_BANHEIRO)
janitor::tabyl(ideb_escolas$IN_COMPUTADOR)
janitor::tabyl(ideb_escolas$IN_LABORATORIO_INFORMATICA)
janitor::tabyl(ideb_escolas$IN_ALIMENTACAO)
janitor::tabyl(ideb_escolas$cat_matematica)
janitor::tabyl(ideb_escolas$cat_portugues)


# Tabeça cruzada  Etapa (Series Iniciais ; Series Finais ; Medio) x target
gmodels::CrossTable(
  ideb_escolas$etapa,
  ideb_escolas$cat_matematica,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("SI (1) SF (2) Medio (3)", "Aprendizado de matemática adequado"),
  format = "SPSS"
)

gmodels::CrossTable(
  ideb_escolas$etapa,
  ideb_escolas$cat_portugues,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("Federal (1) Estadual (2) Municipal (3)", "Aprendizado de portugues adequado"),
  format = "SPSS"
)


# Tabeça cruzada  Dependencia (Federal Estadual Municipal) x target
gmodels::CrossTable(
  ideb_escolas$TP_DEPENDENCIA,
  ideb_escolas$cat_matematica,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("Federal (1) Estadual (2) Municipal (3)", "Aprendizado de matemática adequado"),
  format = "SPSS"
)

gmodels::CrossTable(
  ideb_escolas$TP_DEPENDENCIA,
  ideb_escolas$cat_portugues,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("Federal (1) Estadual (2) Municipal (3)", "Aprendizado de portugues adequado"),
  format = "SPSS"
)

# Tabeça cruzada  Localizacao (urbana rural) x target
gmodels::CrossTable(
  ideb_escolas$TP_LOCALIZACAO,
  ideb_escolas$cat_matematica,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("urbana (1) rural (2)", "Aprendizado de matemática adequado"),
  format = "SPSS"
)

gmodels::CrossTable(
  ideb_escolas$TP_LOCALIZACAO,
  ideb_escolas$cat_portugues,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("urbana (1) rural (2)", "Aprendizado de portugues adequado"),
  format = "SPSS"
)

# Tabeça cruzada  regiao (Norte;Nordeste;Sudeste;Sul;Centro-Oeste) x target
gmodels::CrossTable(
  ideb_escolas$cod_regiao,
  ideb_escolas$cat_matematica,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("NO(1) NE(2) SE(3) S(4) CO(5)", "Aprendizado de matemática adequado"),
  format = "SPSS"
)

gmodels::CrossTable(
  ideb_escolas$cod_regiao,
  ideb_escolas$cat_portugues,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("NO(1) NE(2) SE(3) S(4) CO(5)", "Aprendizado de portugues adequado"),
  format = "SPSS"
)

# Tabeça cruzada biblioteca x target

gmodels::CrossTable(
  ideb_escolas$IN_BIBLIOTECA,
  ideb_escolas$cat_matematica,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("Possui biblioteca", "Aprendizado de matemática adequado"),
  format = "SPSS"
)

gmodels::CrossTable(
  ideb_escolas$IN_BIBLIOTECA,
  ideb_escolas$cat_portugues,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("Possui biblioteca", "Aprendizado de portugues adequado"),
  format = "SPSS"
)


# Tabeça cruzada computador x target

gmodels::CrossTable(
  ideb_escolas$IN_COMPUTADOR,
  ideb_escolas$cat_matematica,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("Possui Computador", "Aprendizado de matemática adequado"),
  format = "SPSS"
)

gmodels::CrossTable(
  ideb_escolas$IN_COMPUTADOR,
  ideb_escolas$cat_portugues,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("Possui Computador", "Aprendizado de portugues adequado"),
  format = "SPSS"
)

# Tabeça cruzada laboratório de info x target

gmodels::CrossTable(
  ideb_escolas$IN_LABORATORIO_INFORMATICA,
  ideb_escolas$cat_matematica,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("Possui laboratório de info", "Aprendizado de matemática adequado"),
  format = "SPSS"
)

gmodels::CrossTable(
  ideb_escolas$IN_LABORATORIO_INFORMATICA,
  ideb_escolas$cat_portugues,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("Possui laboratório de info", "Aprendizado de portugues adequado"),
  format = "SPSS"
)

# Tabeça cruzada alimentação x target

gmodels::CrossTable(
  ideb_escolas$IN_ALIMENTACAO,
  ideb_escolas$cat_matematica,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("fornece alimentação", "Aprendizado de matemática adequado"),
  format = "SPSS"
)

gmodels::CrossTable(
  ideb_escolas$IN_ALIMENTACAO,
  ideb_escolas$cat_portugues,
  expected=F,
  chisq = T,
  prop.r = T,
  prop.c = T,
  fisher = F,
  dnn = c("fornece alimentação", "Aprendizado de portugues adequado"),
  format = "SPSS"
)

# gravando arquivo no disco somente com features para ML
escolas2ML <- ideb_escolas %>% 
  filter(!is.na(ano)) %>% 
  select(
    cod_regiao,
    etapa,
    TP_DEPENDENCIA,
    TP_LOCALIZACAO,
    IN_BIBLIOTECA,
    IN_COMPUTADOR,
    IN_LABORATORIO_INFORMATICA,
    IN_ALIMENTACAO,
    cat_portugues,
    cat_matematica
  ) 

#verificando a estrutura do dataset
dplyr::glimpse(escolas2ML)

#salvando o dataset
save(escolas2ML, file ="./models/escolas2ML.rda")

