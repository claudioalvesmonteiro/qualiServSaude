# Title     : Data Processing PORDATA
# Objective : Coleta e Processamento das Tabelas do PORDATA
# Created by: Claudio
# Created on: 20/05/18

# carregar pacotes
library(RCurl); library(readxl); library(dplyr); library(plyr); library(stringi); library(stringr)

# definir diretorio
setwd("dados/coleta2")

# carregar dados
#temp = list.files(pattern="*.xlsx") # tabelas separadas
#for (i in 1:length(temp)) assign(temp[i], read_excel(temp[i]))

temp = list.files(pattern="*.xlsx") # tabelas juntas
tabelas = lapply(temp, read_excel)

#=========================#
# clean data 
#========================#

funcManiTab1 <- function(data){
  # renomear tabela
  nometab <- colnames(data)[3]%>%
    tolower()%>%
    str_replace_all("sns: ", "")%>%
    str_replace_all(" ", "_")%>%
    str_replace_all("\\(", "")%>%
    str_replace_all("\\)", "")%>%
    str_replace_all("-", "_")%>%
    str_replace_all(":", "_")%>%
    stri_trans_general("Latin-ASCII")

  # selecionar colunas sem missing cases completa e atribuir nome
  data = data[, !sapply(data, function(col) sum(is.na(col)) == length(col)), drop = F]
  return(list(nometab, data))
}

# executar funcao
for (i in seq(1:length(tabelas))){
  assign(funcManiTab1(tabelas[[i]])[[1]], funcManiTab1(tabelas[[i]])[[2]])
}

#for (i in seq(1:length(tabelas))){
#  tabelas[[i]] = tabelas[[i]][tabelas[[i]]$X__1 == "Município" &
#                                !is.na(tabelas[[i]]$X__2),]
#}

#====================#
# funcManiTab2 

funcManiTab2 <- function(data, init, end, info){
  # seleciona apenas municipios e anos de 2009 a 2012 
  data = data[data$X__1 == "Município" & !is.na(data$X__2), c(2, init:end)] 
  # renomeia colunas
  colnames(data) = c("municipio", 2009, 2010, 2011, 2012)
  # informacao de entrada
  data$informacao = info
  # retorna a base manipulada
  return(data)
}

#==== pordata 001  e 003 - numero de habitantes por medico e farmaceutico ====#

# medico
pordata001 <- funcManiTab2(habitantes_por_medico_e_farmaceutico, 4, 7, "habitantes_por_medico")
# farmaceutico
pordata003 <- funcManiTab2(habitantes_por_medico_e_farmaceutico, 13, 16, "habitantes_por_farmaceutico")

#==== pordata 004 - numero de consultas por habitante (centros de saude) ====#
pordata004 <- funcManiTab2(consultas_medicas_nos_centros_de_saude_por_habitante_1993_2012, 6, 9, "consultas_por_habitante_centrosaude")

#==== pordata 005 - pessoal ao servico nos centros de saude ====#

# total
pordata005_01 <- funcManiTab2(pessoal_ao_servico_nos_centros_de_saude__total_e_por_tipo_de_pessoal_ao_servico_1999_2012, 6, 9, "pessoal_ao_servico_centrosaude_total")
# medicos
pordata005_02 <- funcManiTab2(pessoal_ao_servico_nos_centros_de_saude__total_e_por_tipo_de_pessoal_ao_servico_1999_2012, 11, 14, "pessoal_ao_servico_centrosaude_medicos")
# enfermeiros
pordata005_03 <- funcManiTab2(pessoal_ao_servico_nos_centros_de_saude__total_e_por_tipo_de_pessoal_ao_servico_1999_2012, 17, 20, "pessoal_ao_servico_centrosaude_enfermeiros")
# total
pordata005_04 <- funcManiTab2(pessoal_ao_servico_nos_centros_de_saude__total_e_por_tipo_de_pessoal_ao_servico_1999_2012, 23, 26, "pessoal_ao_servico_centrosaude_outros")

#==== Pordata 006 - pessoal ao servico nos hospitais ====#

# total
pordata006_01 <- funcManiTab2(pessoal_ao_servico_nos_hospitais__total_e_por_tipo_de_pessoal_ao_servico, 5, 8, "pessoal_ao_servico_hospitais_total")
# medicos
pordata006_02 <- funcManiTab2(pessoal_ao_servico_nos_hospitais__total_e_por_tipo_de_pessoal_ao_servico, 15, 18, "pessoal_ao_servico_hospitais_medicos")
# enfermeiros
pordata006_03 <- funcManiTab2(pessoal_ao_servico_nos_hospitais__total_e_por_tipo_de_pessoal_ao_servico, 25, 28, "pessoal_ao_servico_hospitais_enfermeiros")
# total
pordata006_04 <- funcManiTab2(pessoal_ao_servico_nos_hospitais__total_e_por_tipo_de_pessoal_ao_servico, 35, 38, "pessoal_ao_servico_hospitais_outros")

#==== Pordata 007 - mortos e feridos em acidentes de viacao ====#

# feridos
pordata007_01 <- funcManiTab2(feridos_e_mortos_em_acidentes_de_viacao, 6, 9, "feridos_em_acidentes_de_viacao")
# mortos 
pordata007_02 <- funcManiTab2(feridos_e_mortos_em_acidentes_de_viacao, 17, 20, "mortos_em_acidentes_de_viacao")

#==== Pordata 008 e 009 - populacao total e grupo etario [MERGIR COM TABELA FINAL] ====#
pordata008 <- populacao_residente__total_e_por_grupo_etario[populacao_residente__total_e_por_grupo_etario$X__1 == "Município" & 
                                                              !is.na(populacao_residente__total_e_por_grupo_etario$X__2),
                                                            c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40)]
# renomear colunas
colnames(pordata008) <- c("municipio", "pop_total", "pop_0a4_anos", "pop_5a9_anos", "pop_10a14_anos", "pop_15a19_anos", "pop_20a24_anos", 
                          "pop_25a29_anos", "pop_30a34_anos", "pop_35a39_anos", "pop_40a44_anos", "pop_45a49_anos", "pop_50a54_anos",
                          "pop_55a59_anos", "pop_60a64_anos", "pop_65a69_anos", "pop_70a74_anos", "pop_75a79_anos", "pop_80a84_anos",
                          "pop_85+anos") 

#==== Pordata 011 - numero de peoes atropelados e mortos ====#

# atropelados
pordata_011_01 <- funcManiTab2(peoes_atropelados__total_e_mortos, 5, 8, "peoes_atropelados_total")
# mortos
pordata_011_02 <- funcManiTab2(peoes_atropelados__total_e_mortos, 15, 18, "peoes_atropelados_mortos")

#==== Pordata 012 - numero de internamentos ====#

# hospitais
pordata_012_01 <- funcManiTab2(internamentos_nos_hospitais, 5, 8, "internamentos_hospitais")
# centros de saude
pordata_012_02<- funcManiTab2(internamentos_nos_centros_de_saude__1993_2012, 6, 9, "internamentos_centrosaude")

#==== Pordata 013 - dias de internamento ====#

# hospitais
pordata_013_01 <- funcManiTab2(dias_de_internamento_nos_hospitais, 5, 8, "dias_de_internamento_hospitais")
# centros de saude
pordata_013_02<- funcManiTab2(dias_de_internamento_nos_centros_de_saude__1993_2012, 5, 8, "dias_de_internamento_nos_centros_de_saude__1993_2012")

#==== Pordata 014 - numero de camas em hospitais por habitante

# hospitais
pordata_014_01<- funcManiTab2(lotacao_dos_hospitais_gerais_e_especializados, 5, 8, "camas_hospitais_porHab")
# centros de saude 
pordata_014_02<- funcManiTab2(lotacao_dos_centros_de_saude_1993_2012, 6, 9, "camas_centrosaude_porHab")

#==== Pordata 015 - numero de urgencias ====#

# hospitais
pordata_015_01 <- funcManiTab2(urgencias_nos_hospitais, 5, 8, "urgencias_hospitais")
# centros de saude
pordata_015_02 <- funcManiTab2(urgencias_nos_centros_de_saude_1999_2012, 5, 8, "urgencias_centrosaude")

#==== Pordata 016 - numero de partos em hospitais ====#
pordata_016 <- funcManiTab2(partos_nos_hospitais__total_e_por_tipo, 5, 8, "partos_nos_hospitais")

#==== Pordata 017 - numero de extensoes dos centros de saude ====#
pordata017 <- extensoes_dos_centros_de_saude_1993_2011[extensoes_dos_centros_de_saude_1993_2011$X__1 == "Município" & 
                                                     !is.na(extensoes_dos_centros_de_saude_1993_2011$X__2),
                                                   c(2,6:8)]
colnames(pordata017) <- c("municipio", 2009, 2010, 2011) # renomear colunas e criar coluna de info
pordata017$informacao <- "extensoes_centrosaude_2011"

#==== Pordata 018 - numero de interrupcoes voluntárias de gravidez ====#
pordata_018 <- funcManiTab2(interrupcoes_voluntarias_da_gravidez_nos_estabelecimentos_de_saude, 4, 7, "interrupcoes_voluntarias_gravidez")

#==== Pordata 019 - numero de estabelecimentos farmaceuticos ====#
pordata_019 <- funcManiTab2(farmacias_e_postos_farmaceuticos_moveis, 5, 8, "estabelecimentos_farmaceuticos")

#===========================#
# combinar tabelas em base
#===========================#

# combinar bases
basePordata <- rbind(pordata001, pordata003, pordata004, pordata005_01, pordata005_02, pordata005_03, pordata005_04, 
                     pordata006_01, pordata006_02, pordata006_03, pordata006_04, pordata007_01, pordata007_02,
                     pordata_011_01, pordata_011_02, pordata_012_01, pordata_012_02, pordata_013_01, pordata_013_02,
                     pordata_014_01, pordata_014_02, pordata_015_01, pordata_015_02, pordata_016,
                     pordata_018, pordata_019)

#==== transformar para wide ====#
library(reshape2)
basePordataWide <- melt(basePordata, id.vars=c("municipio", "informacao"))
colnames(basePordataWide)[3] <- "ano"
basePordataWide$value <- as.numeric(basePordataWide$value)

#==== tranformar para long ====#
basePordataLong<- reshape(basePordataWide, idvar=c("municipio", "ano"), timevar="informacao", direction="wide")

#==== mergir e salvar ====#
basePordatax <- merge(basePordata, pordata008, by = "municipio") 
basePordataWidex <- merge(basePordataWide, pordata008, by = "municipio") 
basePordataLongx <- merge(basePordataLong, pordata008, by = "municipio") 

library(xlsx)
write.xlsx(basePordatax, "basePordata1.xls")
write.xlsx(basePordataWidex, "basePordata2.xls")
write.xlsx(basePordataLongx, "basePordata3.xls")



#








