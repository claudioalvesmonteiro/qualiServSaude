# Title     : Data Processing PORDATA
# Objective : Coleta e Processamento das Tabelas do PORDATA
# Created by: pacha
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
  data = data[data$X__1 == "Município" & !is.na(data$X__2), c(2, init:end)]
  colnames(data) = c("municipio", 2009, 2010, 2011, 2012)
  data$informacao = info
  return(data)
}

#==== pordata 001  e 003 - numero de habitantes por medico e farmaceutico ====#

# selecionar municipios, nao missing e medicos 
pordata001 <- habitantes_por_medico_e_farmaceutico[habitantes_por_medico_e_farmaceutico$X__1 == "Município" & 
                                               !is.na(habitantes_por_medico_e_farmaceutico$X__2),
                                             c(2,4:7)]
colnames(pordata001) <- c("municipio", 2009, 2010, 2011, 2012) # renomear colunas e criar coluna de info
pordata001$informacao <- "numero_habitantes_por_medico"

# selecionar municipios, nao missing e farmaceuticos 
pordata003 <- habitantes_por_medico_e_farmaceutico[habitantes_por_medico_e_farmaceutico$X__1 == "Município" & 
                                               !is.na(habitantes_por_medico_e_farmaceutico$X__2),
                                             c(2,13:16)]
colnames(pordata003) <- c("municipio", 2009, 2010, 2011, 2012) # renomear colunas e criar coluna de info
pordata003$informacao <- "numero_habitantes_por_farmaceutico"

#==== pordata 004 - numero de consultas por habitante (centros de saude) ====#
pordata004 <- funcManiTab2(consultas_medicas_nos_centros_de_saude_por_habitante_1993_2012, 
                           6, 9, "numero_de_consultas_por_habitante_centrosaude")

#==== pordata 005 - pessoal ao servico nos centros de saude ====#

# total
pordata005_01 <- funcManiTab2(`pessoal_ao_servico_nos_centros_de_saude:_total_e_por_tipo_de_pessoal_ao_servico_1999_2012`, 
                              6, 9, "pessoal_ao_servico_centrosaude_total")
# medicos
pordata005_02 <- funcManiTab2(`pessoal_ao_servico_nos_centros_de_saude:_total_e_por_tipo_de_pessoal_ao_servico_1999_2012`, 
                              11, 14, "pessoal_ao_servico_centrosaude_medicos")
# enfermeiros
pordata005_03 <- funcManiTab2(`pessoal_ao_servico_nos_centros_de_saude:_total_e_por_tipo_de_pessoal_ao_servico_1999_2012`, 
                              17, 20, "pessoal_ao_servico_centrosaude_enfermeiros")
# total
pordata005_04 <- funcManiTab2(`pessoal_ao_servico_nos_centros_de_saude:_total_e_por_tipo_de_pessoal_ao_servico_1999_2012`, 
                              23, 26, "pessoal_ao_servico_centrosaude_outros")

#==== pordata 006 - pessoal ao servico nos hospitais ====#
# total
pordata006_01 <- funcManiTab2(`pessoal_ao_servico_nos_hospitais:_total_e_por_tipo_de_pessoal_ao_servico`, 
                              5, 8, "pessoal_ao_servico_centrosaude_total")
# medicos
pordata006_02 <- funcManiTab2(`pessoal_ao_servico_nos_hospitais:_total_e_por_tipo_de_pessoal_ao_servico`, 
                              15, 18, "pessoal_ao_servico_centrosaude_medicos")
# enfermeiros
pordata006_03 <- funcManiTab2(`pessoal_ao_servico_nos_hospitais:_total_e_por_tipo_de_pessoal_ao_servico`,
                              25, 28, "pessoal_ao_servico_centrosaude_enfermeiros")
# total
pordata006_04 <- funcManiTab2(`pessoal_ao_servico_nos_hospitais:_total_e_por_tipo_de_pessoal_ao_servico`, 
                              35, 38, "pessoal_ao_servico_centrosaude_outros")

#==== pordata 007 - mortos e feridos em acidentes de viacao ====#
# feridos
pordata007_01 <- funcManiTab2(feridos_e_mortos_em_acidentes_de_viacao, 
                              6, 9, "feridos_em_acidentes_de_viacao")
# mortos 
pordata007_02 <- funcManiTab2(feridos_e_mortos_em_acidentes_de_viacao, 
                              17, 20, "mortos_em_acidentes_de_viacao")




