#### - Diretório de trabalho - ####
setwd("C:\\Users\\pedro\\OneDrive\\mun_prioritarios_covid")
getwd()


# Pacotes
pacotes <- c("dplyr", "lubridate", "tidyr", "stringr", "readr", 
             "foreign", "knitr", "kableExtra", "PROMETHEE")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}



# Bancos de dados
bd17 <- read.dbf("HANSN17.dbf", as.is = F)
bd18 <- read.dbf("HANSN18.dbf", as.is = F)
bd19 <- read.dbf("HANSN19.dbf", as.is = F)
bd20 <- read.dbf("HANSN20.dbf", as.is = F)
bd21 <- read.dbf("HANSN21.dbf", as.is = F)

mun <- read.csv2(file = "mun.csv", sep = ';', dec = ',', header = T)


### --- FASE 1: tratamento dos dados ---

# União dos BDs
bd <- bind_rows(bd17,
                bd18,
                bd19,
                bd20,
                bd21)

rm(bd17, bd18, bd19, bd20, bd21, excluir, pacotes) # Remove os bancos de dados


# Exclusão de variáveis
excluir <- c("NU_DDD_TEL", "NU_TELEFON", "NU_CEP", "CEP", "NU_PRONTUA", 
             "ID_CNS_SUS", "ID_GEO1", "NOBAIRROAT", "ID_OCUPA_N", 
             "TP_NOT", "ID_AGRAVO", "ID_REGIONA", "SEM_DIAG", "NM_PACIENT",
             "FONETICA_N", "SOUNDEX", "NM_MAE_PAC", "ID_RG_RESI", "ID_DISTRIT",
             "NM_BAIRRO", "ID_LOGRADO", "NM_LOGRADO", "NU_NUMERO", "NM_COMPLEM",
             "ID_GEO2", "NM_REFEREN", "ID_PAIS", "NDUPLIC_N", "DT_TRANSUS",
             "DT_TRANSDM", "DT_TRANSFM", "DT_TRANSRS", "DT_TRANSSE", 
             "DT_TRANSSM", "DT_TRANSRM", "NU_LOTE_V", "NU_LOTE_H", "CS_FLXRET",
             "FLXRECEBI", "IDENT_MICR", "MIGRADO_W", "NU_NOTI_AT", "DISTRIT_AT",
             "BAIRROAT", "IN_VINCULA", "NU_LOTE_IA", "ID_MUNICIP", "ID_UNIDADE",
             "CS_ESCOL_N", "ID_BAIRRO",
             "ID_MUNI_AT", "NU_NOT_AT", "ID_UNID_AT", "NU_ANO")


bd <- bd[,!(names(bd) %in% excluir)]


# Criação da variável ufres a partir da var ID_MN_RESI
class(bd$ID_MN_RESI)

bd$ID_MN_RESI <- as.character(as.factor(bd$ID_MN_RESI))

bd <- bd %>% mutate(uf_resi = str_sub(bd$ID_MN_RESI, 0, 2))

bd$uf_resi <- as.character(as.factor(bd$uf_resi))


# Criação da variável "ANO_DIAG"
bd$DT_DIAG <- as.Date(bd$DT_DIAG, format = "%d/%m/%Y")

ANO_DIAG <- year(bd$DT_DIAG)

bd[,"ANO_DIAG"] <- ANO_DIAG

bd <- filter(bd, ANO_DIAG >= 2017 & ANO_DIAG <= 2021)



### --- FASE 2: Criar DF com casos de hanseníase ---

# Caso novo geral
casos_novos <- bd %>% 
  filter(MODOENTR == 1, ANO_DIAG %in% c(2019, 2020, 2021), TPALTA_N != 8) %>% 
  group_by(ID_MN_RESI, ANO_DIAG) %>%
  summarize(casos_19 = n()) %>% 
  spread(ANO_DIAG, casos_19) %>%
  rename(casos19 = 2,
         casos20 = 3,
         casos21 = 4)


casos_novos %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)


# Caso novo menores de 15 anos
menor_15 <- bd %>% 
  filter(MODOENTR == 1, ANO_DIAG %in% c(2019, 2020, 2021), 
         NU_IDADE_N %in% c(1000:4014), TPALTA_N != 8) %>% 
  group_by(ID_MN_RESI, ANO_DIAG) %>%
  summarize(menor_19 = n()) %>%
  spread(ANO_DIAG, menor_19) %>% 
  rename(menor19 = 2,
         menor20 = 3,
         menor21 = 4)


menor_15 %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)


# Caso novo GIF 2
gif2 <- bd %>% 
  filter(MODOENTR == 1, ANO_DIAG %in% c(2019, 2020, 2021),
         AVALIA_N == 2, TPALTA_N != 8) %>% 
  group_by(ID_MN_RESI, ANO_DIAG) %>%
  summarize(gif2 = n()) %>% 
  spread(ANO_DIAG, gif2) %>% 
  rename(grau19 = 2,
         grau20 = 3,
         grau21 = 4)


gif2 %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)


### - Casos novos avaliados - ###
aval <- bd %>% 
  filter(MODOENTR == 1, ANO_DIAG %in% c(2019, 2020, 2021),
         AVALIA_N %in% c(0,1,2), TPALTA_N != 8) %>% 
  group_by(ID_MN_RESI, ANO_DIAG) %>%
  summarize(aval = n()) %>% 
  spread(ANO_DIAG, aval) %>% 
  rename(aval19 = 2,
         aval20 = 3,
         aval21 = 4)


aval %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)


# Juntar os casos de hanseníase em um DF
mun <- mun %>% mutate(cod_ibge = str_sub(mun$Município, 0, 6)) # variável código IBGE

j1 <- full_join(mun, casos_novos, by = c("cod_ibge" = "ID_MN_RESI"))
j2 <- full_join(j1, menor_15, by = c("cod_ibge" = "ID_MN_RESI"))
j3 <- full_join(j2, gif2, by = c("cod_ibge" = "ID_MN_RESI"))
j4 <- full_join(j3, aval, by = c("cod_ibge" = "ID_MN_RESI"))


############ 2019
### - Contatos examinados paucibacilares - ###
exam_pb19 <- bd %>% 
  filter(MODOENTR == 1, 
         ANO_DIAG == 2018, 
         CLASSATUAL == 1, 
         ESQ_ATU_N == 1, 
         TPALTA_N != 8) %>% 
  select(MUNIRESAT, CONTEXAM)

exam_pb19 <- exam_pb19 %>% 
  mutate_all(replace_na, 0)

exam_pb19 <- exam_pb19 %>% 
  group_by(MUNIRESAT) %>% 
  summarise(exam_pb_tt = sum(CONTEXAM)) %>% 
  ungroup () %>% 
  droplevels(.)


### - Contatos examinados multibacilares - ###
exam_mb19 <- bd %>% 
  filter(MODOENTR == 1, 
         ANO_DIAG == 2017, 
         CLASSATUAL == 2, 
         ESQ_ATU_N == 2,
         TPALTA_N != 8) %>% 
  select(MUNIRESAT, CONTEXAM)


exam_mb19 <- exam_mb19 %>% 
  mutate_all(replace_na, 0)

exam_mb19 <- exam_mb19 %>% 
  group_by(MUNIRESAT) %>% 
  summarise(exam_mb_tt = sum(CONTEXAM)) %>% 
  ungroup () %>% 
  droplevels(.)


# Join e soma dos contatos examinados
ctt_exam19 <- full_join(exam_pb19, exam_mb19) %>% 
  mutate(exam_pb_tt + exam_mb_tt) %>% 
  rename(exam19 = 4) %>% 
  select(MUNIRESAT, exam19)


################### 2020
### - Contatos examinados paucibacilares - ###
exam_pb20 <- bd %>% 
  filter(MODOENTR == 1, 
         ANO_DIAG == 2019, 
         CLASSATUAL == 1, 
         ESQ_ATU_N == 1, 
         TPALTA_N != 8) %>% 
  select(MUNIRESAT, CONTEXAM)

exam_pb20 <- exam_pb20 %>% 
  mutate_all(replace_na, 0)

exam_pb20 <- exam_pb20 %>% 
  group_by(MUNIRESAT) %>% 
  summarise(exam_pb_tt = sum(CONTEXAM)) %>% 
  ungroup () %>% 
  droplevels(.)


### - Contatos examinados multibacilares - ###
exam_mb20 <- bd %>% 
  filter(MODOENTR == 1, 
         ANO_DIAG == 2018, 
         CLASSATUAL == 2, 
         ESQ_ATU_N == 2,
         TPALTA_N != 8) %>% 
  select(MUNIRESAT, CONTEXAM)


exam_mb20 <- exam_mb20 %>% 
  mutate_all(replace_na, 0)

exam_mb20 <- exam_mb20 %>% 
  group_by(MUNIRESAT) %>% 
  summarise(exam_mb_tt = sum(CONTEXAM)) %>% 
  ungroup () %>% 
  droplevels(.)


# Join e soma dos contatos examinados
ctt_exam20 <- full_join(exam_pb20, exam_mb20) %>% 
  mutate(exam_pb_tt + exam_mb_tt) %>% 
  rename(exam20 = 4) %>% 
  select(MUNIRESAT, exam20)


################### 2021
### - Contatos examinados paucibacilares - ###
exam_pb21 <- bd %>% 
  filter(MODOENTR == 1, 
         ANO_DIAG == 2020, 
         CLASSATUAL == 1, 
         ESQ_ATU_N == 1, 
         TPALTA_N != 8) %>% 
  select(MUNIRESAT, CONTEXAM)

exam_pb21 <- exam_pb21 %>% 
  mutate_all(replace_na, 0)

exam_pb21 <- exam_pb21 %>% 
  group_by(MUNIRESAT) %>% 
  summarise(exam_pb_tt = sum(CONTEXAM)) %>% 
  ungroup () %>% 
  droplevels(.)


### - Contatos examinados multibacilares - ###
exam_mb21 <- bd %>% 
  filter(MODOENTR == 1, 
         ANO_DIAG == 2019, 
         CLASSATUAL == 2, 
         ESQ_ATU_N == 2,
         TPALTA_N != 8) %>% 
  select(MUNIRESAT, CONTEXAM)


exam_mb21 <- exam_mb21 %>% 
  mutate_all(replace_na, 0)

exam_mb21 <- exam_mb21 %>% 
  group_by(MUNIRESAT) %>% 
  summarise(exam_mb_tt = sum(CONTEXAM)) %>% 
  ungroup () %>% 
  droplevels(.)


# Join e soma dos contatos examinados
ctt_exam21 <- full_join(exam_pb21, exam_mb21) %>% 
  mutate(exam_pb_tt + exam_mb_tt) %>% 
  rename(exam21 = 4) %>% 
  select(MUNIRESAT, exam21)



########### 2019
### - Contatos registrados paucibacilares - ###
reg_pb19 <- bd %>% 
  filter(MODOENTR == 1, 
         ANO_DIAG == 2018, 
         CLASSATUAL == 1, 
         ESQ_ATU_N == 1, 
         TPALTA_N != 8) %>% 
  select(MUNIRESAT, CONTREG)


reg_pb19 <- reg_pb19 %>% 
  mutate_all(replace_na, 0)


reg_pb19 <- reg_pb19 %>% 
  group_by(MUNIRESAT) %>% 
  summarise(reg_pb_tt = sum(CONTREG)) %>% 
  ungroup () %>% 
  droplevels(.)



### - Contatos registrados multibacilares - ###
reg_mb19 <- bd %>% 
  filter(MODOENTR == 1, 
         ANO_DIAG == 2017, 
         CLASSATUAL == 2, 
         ESQ_ATU_N == 2, 
         TPALTA_N != 8) %>% 
  select(MUNIRESAT, CONTREG)

reg_mb19 <- reg_mb19 %>% 
  mutate_all(replace_na, 0)


reg_mb19 <- reg_mb19 %>% 
  group_by(MUNIRESAT) %>% 
  summarise(reg_mb_tt = sum(CONTREG)) %>% 
  ungroup () %>% 
  droplevels(.)


# Join e soma dos contatos examinados
ctt_reg19 <- full_join(reg_pb19, reg_mb19) %>% 
  mutate(reg_pb_tt + reg_mb_tt) %>% 
  rename(reg19 = 4) %>% 
  select(MUNIRESAT, reg19)



########### 2020
### - Contatos registrados paucibacilares - ###
reg_pb20 <- bd %>% 
  filter(MODOENTR == 1, 
         ANO_DIAG == 2018, 
         CLASSATUAL == 1, 
         ESQ_ATU_N == 1, 
         TPALTA_N != 8) %>% 
  select(MUNIRESAT, CONTREG)

reg_pb20 <- reg_pb20 %>% 
  mutate_all(replace_na, 0)

reg_pb20 <- reg_pb20 %>% 
  group_by(MUNIRESAT) %>% 
  summarise(reg_pb_tt = sum(CONTREG)) %>% 
  ungroup () %>% 
  droplevels(.)



### - Contatos registrados multibacilares - ###
reg_mb20 <- bd %>% 
  filter(MODOENTR == 1, 
         ANO_DIAG == 2017, 
         CLASSATUAL == 2, 
         ESQ_ATU_N == 2, 
         TPALTA_N != 8) %>% 
  select(MUNIRESAT, CONTREG)

reg_mb20 <- reg_mb20 %>% 
  mutate_all(replace_na, 0)


reg_mb20 <- reg_mb20 %>% 
  group_by(MUNIRESAT) %>% 
  summarise(reg_mb_tt = sum(CONTREG)) %>% 
  ungroup () %>% 
  droplevels(.)


# Join e soma dos contatos examinados
ctt_reg20 <- full_join(reg_pb20, reg_mb20) %>% 
  mutate(reg_pb_tt + reg_mb_tt) %>% 
  rename(reg20 = 4) %>% 
  select(MUNIRESAT, reg20)


########### 2020
### - Contatos registrados paucibacilares - ###
reg_pb21 <- bd %>% 
  filter(MODOENTR == 1, 
         ANO_DIAG == 2018, 
         CLASSATUAL == 1, 
         ESQ_ATU_N == 1, 
         TPALTA_N != 8) %>% 
  select(MUNIRESAT, CONTREG)


reg_pb21 <- reg_pb21 %>% 
  mutate_all(replace_na, 0)


reg_pb21 <- reg_pb21 %>% 
  group_by(MUNIRESAT) %>% 
  summarise(reg_pb_tt = sum(CONTREG)) %>% 
  ungroup () %>% 
  droplevels(.)



### - Contatos registrados multibacilares - ###
reg_mb21 <- bd %>% 
  filter(MODOENTR == 1, 
         ANO_DIAG == 2017, 
         CLASSATUAL == 2, 
         ESQ_ATU_N == 2, 
         TPALTA_N != 8) %>% 
  select(MUNIRESAT, CONTREG)

reg_mb21 <- reg_mb21 %>% 
  mutate_all(replace_na, 0)


reg_mb21 <- reg_mb21 %>% 
  group_by(MUNIRESAT) %>% 
  summarise(reg_mb_tt = sum(CONTREG)) %>% 
  ungroup () %>% 
  droplevels(.)


# Join e soma dos contatos examinados
ctt_reg21 <- full_join(reg_pb21, reg_mb21) %>% 
  mutate(reg_pb_tt + reg_mb_tt) %>% 
  rename(reg21 = 4) %>% 
  select(MUNIRESAT, reg21)




j5 <- full_join(j4, ctt_exam19, by = c("cod_ibge" = "MUNIRESAT"))
j6 <- full_join(j5, ctt_exam20, by = c("cod_ibge" = "MUNIRESAT"))
j7 <- full_join(j6, ctt_exam21, by = c("cod_ibge" = "MUNIRESAT"))
j8 <- full_join(j7, ctt_reg19, by = c("cod_ibge" = "MUNIRESAT"))
j9 <- full_join(j8, ctt_reg20, by = c("cod_ibge" = "MUNIRESAT"))
j10 <- full_join(j9, ctt_reg21, by = c("cod_ibge" = "MUNIRESAT"))


dados <- j10
glimpse(dados)

rm(aval, bd, casos_novos, ctt_exam19, ctt_exam20, ctt_exam21,
   ctt_reg19, ctt_reg20, ctt_reg21, exam_mb19, exam_mb20,
   exam_mb21, exam_pb19, exam_pb20, exam_pb21, gif2, j1, j2, j3,
   j4, j5, j6, j7, j8, j9, j10, menor_15, mun, reg_mb19,
   reg_mb20, reg_mb21, reg_pb19, reg_pb20, reg_pb21, ANO_DIAG, excluir)


dados <- dados %>% 
  mutate_all(replace_na, 0)


dados <- dados %>% mutate(cont_exam19 = round(((exam19 / reg19) * 100), 2))
dados <- dados %>% mutate(cont_exam20 = round(((exam20 / reg20) * 100), 2))
dados <- dados %>% mutate(cont_exam21 = round(((exam19 / reg21) * 100), 2))
dados <- dados %>% mutate(prop_gif19 = round(((grau19 / aval19) * 100), 2))
dados <- dados %>% mutate(prop_gif20 = round(((grau20 / aval20) * 100), 2))
dados <- dados %>% mutate(prop_gif21 = round(((grau19 / aval21) * 100), 2))


dados <- dados %>% select(everything(), -c(grau19, grau20, grau21,
                                           aval19, aval20, aval21,
                                           exam19, exam20, exam21,
                                           reg19, reg20, reg21))


dados %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)


dados <- dados[-c(5571, 5572, 5573, 5574, 5575, 5576, 5577, 5578,
                  5579, 5580, 5581),]


dados <- dados %>% rename(mun = 1,
                          pop_2019 = 2,
                          pop_2020 = 3) %>% 
  select(mun, cod_ibge, pop_2019, pop_2020, everything())



# Converter NAs em 0
dados <- dados %>% 
  mutate_all(replace_na, 0)



# Cálculo da diferença
difer <- function(valor1, valor2) {
  diference <- valor2 - valor1
  return(diference)
}


difcasos_1 <- difer(dados$casos19, dados$casos20)
difcasos_2 <- difer(dados$casos20, dados$casos21)


dados$difcasos_1 <- difcasos_1
dados$difcasos_2 <- difcasos_2



difmenor_1 <- difer(dados$menor19, dados$menor20)
difmenor_2 <- difer(dados$menor20, dados$menor21)


dados$difmenor_1 <- difmenor_1
dados$difmenor_2 <- difmenor_2



difexam_1 <- difer(dados$cont_exam19, dados$cont_exam20)
difexam_2 <- difer(dados$cont_exam20, dados$cont_exam21)


dados$difexam_1 <- difexam_1
dados$difexam_2 <- difexam_2



difgif_1 <- difer(dados$prop_gif19, dados$prop_gif20)
difgif_2 <- difer(dados$prop_gif20, dados$prop_gif21)


dados$difgif_1 <- difgif_1
dados$difgif_2 <- difgif_2



dados %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)



dados2 <- dados %>% select(casos19,
                           difcasos_1,
                           difcasos_2,
                           difmenor_1,
                           difmenor_2,
                           difexam_1,
                           difexam_2,
                           difgif_1,
                           difgif_2)



# Variável tamanho do município
dados <- dados %>%
  mutate(porte_mun = case_when(pop_2020 >= 50001 ~ 'grupo 3',
                               pop_2020 >= 20001  & pop_2020 <= 50000 ~ 'grupo 2',
                               pop_2020 <= 20000 ~ 'grupo 1'))


table(dados$porte_mun)


# DF para cada grupo
# Grupo 1
bd_g1 <- dados %>% filter(porte_mun == "grupo 1")

# Grupo 2
bd_g2 <- dados %>% filter(porte_mun == "grupo 2")

# Grupo 3
bd_g3 <- dados %>% filter(porte_mun == "grupo 3")





dados <- dados %>% select(casos19,
                          difcasos_1,
                          difcasos_2,
                          difmenor_1,
                          difmenor_2,
                          difexam_1,
                          difexam_2,
                          difgif_1,
                          difgif_2)


dados3 <- bd_g3 %>% select(casos19,
                           difcasos_1,
                           difcasos_2,
                           difmenor_1,
                           difmenor_2,
                           difexam_1,
                           difexam_2,
                           difgif_1,
                           difgif_2)



is.na(dados)<-sapply(dados, is.infinite)
dados[is.na(dados)] <- 0


