## LIBRERIAS NECESARIAS --------------------------------------------------------
install.packages("RODBC")
install.packages("DBI")
install.packages("RJDBC")
install.packages("DatabaseConnector")
install.packages("DatabaseConnectorJars")
install.packages("XML")
install.packages("xml2")

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_202')
library(RODBC)
library(rJava)
library(DBI)
library(RJDBC)
library(DatabaseConnector)
library(DatabaseConnectorJars)
library(XML)
library(xml2)
library(tidyr)
library(tidyverse)
library(dplyr)
library(xml2)
library(XML)
library(tibble)
source("operaciones.R") # Loading functions in operaciones.R (set path to FILE)

## OBTENCION DATOS DEL XML -----------------------------------------------------
setwd("") # Loading conn.xml (set path to DIRECTORY)


## datos de conexion obtenidos del xml
y<-as_list(read_xml("conn.xml"))
xml_df = as_tibble(y) %>%unnest_longer(ETLconfiguration)
lp_wider = xml_df %>%filter(ETLconfiguration_id == "connection") %>%unnest_wider(ETLconfiguration)
conexiones= lp_wider %>% unnest(cols = names(.)) %>% unnest(cols = names(.)) %>% readr::type_convert()
conexiones<-data.frame(conexiones)
row.names(conexiones)<-conexiones$alias
conexiones$alias<-NULL
conexiones$ETLconfiguration_id<-NULL

## CONEXION BBDD HCIS
drv2 <- JDBC("oracle.jdbc.OracleDriver","D:\\Documents and Settings\\71355066G\\Escritorio\\Workbench-Build115\\ojdbc7-12.1.0.2.jar",identifier.quote="")
conn2 <- dbConnect(drv2, "jdbc:oracle:thin:@10.35.49.17:1522:explohdoc", "EXPLOH12O", "eXP10H12O")

## OBTENCION DE LAS TABLAS FUENTES Y LOS ARGUMENTOS DE LAS OPERACIONES
y<-as_list(read_xml("conn.xml"))
xml_df = as_tibble(y) %>%unnest_longer(ETLconfiguration)
lp_wider = xml_df %>%filter(ETLconfiguration_id == "source") %>%unnest_wider(ETLconfiguration)
fuentes= lp_wider %>% unnest(cols = names(.)) %>% unnest(cols = names(.)) %>% readr::type_convert()
fuentes<-data.frame(fuentes)
vars<-unlist(fuentes$attributes)

y<-as_list(read_xml("conn.xml"))
xml_df = as_tibble(y) %>%unnest_longer(ETLconfiguration)
lp_wider = xml_df %>%filter(ETLconfiguration_id == "operation") %>%unnest_wider(ETLconfiguration)
operaciones= lp_wider %>% unnest(cols = names(.)) %>% unnest(cols = names(.)) %>% readr::type_convert()
operaciones<-data.frame(operaciones)
ext<-operaciones %>% filter(type=="Extraction")
sel<-operaciones %>% filter(type=="Selection")
tr<-operaciones %>% filter(type=="Transformation")

## CREACION DE LA QUERY PARA LA EXTRACCION
vars<-paste(",",vars, " ", collapse="", sep="")
vars<-str_sub(vars, start = 2)
query<-paste("select ",vars, "from ",ext$input," WHERE " ,ext$input,".numerohc"," IN ( SELECT ",ext$argument,".numerohc"," FROM ",ext$argument,")", sep="", collapse = "")
db<-dbGetQuery(conn2,query)
tbs<-list()
tbs[[1]]<-db
map_obx <- dbGetQuery(conn2, "select OBX, DESCR_LARGA, UDS from IMAS12_CONCEPT_OBX")


## OPERACIONES
m<-nrow(sel)
for(i in 1:m){
  tb_inicio<-tbs[[1]]
  op<-sel$id[i]
  var<-sel$concept[i]
  cr<-sel$argument[i]
  tb_final<-operations(tb_inicio,op,var,cr)
  assign(sel$output[i],tb_final)
  tbs[[1]] <- tb_final
}

m<-nrow(tr)
for(i in 1:m){
  tb_inicio<-tbs[[1]]
  op<-tr$id[i]
  var<-tr$concept[i]
  cr<-tr$argument[i]
  tb_final<-operations(tb_inicio,op,var,cr)
  assign(tr$output[i],tb_final)
  tbs[[1]] <- tb_final
}
