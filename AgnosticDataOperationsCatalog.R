## Functions -------------------------------------------------------------------
operations<-function(input,operation,concept,argument){
  switch(operation,
    #SELECTIONS
    #SELECTION WITH REFERENCE
    S.1.1={ #Selection by CV
      argument<-unlist(str_split(argument,","))
      output<-input[(input[[concept]]%in%argument),]
      },
    S.1.2={ #Selection of data previous to date 
      if(str_detect(argument,",")==TRUE){
        argument<-unlist(str_split(argument,","))
      }
      argument<-as.Date(argument)
      input[[concept]]<-as.Date(input[[concept]])
      output<-filter(input, input[[concept]]<argument)},
    S.1.3={ #Selection of data after date 
      if(str_detect(argument,",")==TRUE){
        argument<-unlist(str_split(argument,","))
      }
      argument<-as.Date(argument)
      input[[concept]]<-as.Date(input[[concept]])
      output<-filter(input, input[[concept]]>argument)},
    S.1.4={ #Selection of data higher than value
      if(str_detect(argument,",")==TRUE){
        argument<-unlist(str_split(argument,","))
      }
      argument<-as.numeric(argument)
      input[[concept]]<-as.numeric(input[[concept]])
      output<-filter(input, input[[concept]]>argument)
      },
    S.1.5={ #Selection of data less than value
      if(str_detect(argument,",")==TRUE){
        argument<-unlist(str_split(argument,","))
      }
      argument<-as.numeric(argument)
      input[[concept]]<-as.numeric(input[[concept]])
      output<-filter(input, input[[concept]]<argument)
    },
    S.1.6={ #Selection of data equal to value
      if(str_detect(argument,",")==TRUE){
        argument<-unlist(str_split(argument,","))
      }
      argument<-as.numeric(argument)
      input[[concept]]<-as.Date(input[[concept]])
      output<-filter(input, input[[concept]]==argument)
    },
    #SELECTION WITHOUT REFERENCE
    S.2.1={ #Selection of most recent datum 
      input[[var]]<-as.Date(input[[var]])
      output<-input %>% group_by(NUMEROHC) %>% arrange(desc(input[[concept]])) %>% slice(1)
    },
    S.2.2={ #Selection of oldest datum 
      input[[concept]]<-as.Date(input[[concept]])
      output<-input %>% group_by(NUMEROHC) %>% arrange(input[[concept]]) %>% slice(1)
    },
    S.2.3={ #Selection of datum with higher value 
      output<-input %>%group_by(NUMEROHC) %>% summarise(VALOR=max(VALOR))
    },
    S.2.4={ #Selection of datum with less value 
      output<-input %>%group_by(NUMEROHC) %>% summarise(VALOR=min(VALOR))
    },
    #TRANSFORMATIONS
    #TRANSFORMATION MAINTAINING MEANING
    T.1.1={ #Change of unit of measure 
      map_obx <- dbGetQuery(conn2, "select OBX, DESCR_LARGA, UDS from IMAS12_CONCEPT_OBX")
      switch(argument,
            cm_m = {
              output <- input %>% inner_join(map_obx, by = "OBX") %>%  mutate(VALOR = as.numeric(VALOR)*0.01,
                                                                    UDS = "m",
                                                                    DESCR_LARGA = "Talla (m)")
            },
            kg_g = {
              output <- input %>% inner_join(map_obx, by = "OBX") %>%  mutate(VALOR = as.numeric(VALOR)*1000,
                                                                              UDS = "g",
                                                                              DESCR_LARGA = "Peso (g)")
            },
            cmh20 = {
              output <- input %>% inner_join(map_obx, by = "OBX") %>%  mutate(VALOR = as.numeric(VALOR)*1.35951,
                                                                              UDS = "cmh20")
            },
            ug_ml = {
              output <- input %>% inner_join(map_obx, by = "OBX") %>%  mutate(VALOR = as.numeric(VALOR)*10,
                                                                              UDS = "ug_ml")
            }
      )
    },
    T.1.2={ #Change of coding system
      print(paste("Operacion en columna", concept))
      a <- levels(factor(input[[concept]]))
      label_factor <- numeric()
      
      for(i in a){
        print(paste("Valor para ", i, ":", sep = ""))
        label_factor <- append(label_factor, as.numeric(readline()))
      }
      
      input[[concept]] <- factor(input[[concept]], a, labels = label_factor)
      output<-input
      
      },
    #TRANSFORMATION ALTERING MEANING
    T.2.1={ #Mathematical operation 
      map_obx <- dbGetQuery(conn2, "select OBX, DESCR_LARGA, UDS from IMAS12_CONCEPT_OBX")
      
      spread_tbl <- input %>% inner_join(map_obx, by = "OBX") %>% select(CODPACI, FECHA, DESCR_LARGA, VALOR) %>% distinct() %>% 
        pivot_wider(names_from = DESCR_LARGA, values_from = VALOR) %>% drop_na()
      
      switch(argument,
             IMC = {
               output <- spread_tbl %>% mutate(IMC = as.numeric(`Peso (kg)`)/(as.numeric(`Talla (cm)`)/100)^2)
             },
             
             Crea_Alb = {
               spread_tbl <- spread_tbl %>% filter(`Creatinina` != "Comentario" & `Albúmina` != "Comentario" & !grepl("c\\(",`Creatinina`) & !grepl("c\\(",`Albúmina`))
               output <- spread_tbl %>% mutate(Crea_Alb = as.numeric(`Creatinina`)/as.numeric(`Albúmina`))
             }
             )
    },
    T.2.2={ #Semantic inference 
      print(paste("Inferencia por debajo de ", argument,":", sep = ""))
      low_value <- readline()
      print(paste("Inferencia mayor o igual que ", argument,":", sep = ""))
      high_value <- readline()
      
      output <- input %>% mutate(INFERENCE = case_when(VALOR < argument ~ low_value,
                                                       VALOR >= argument ~ high_value))
    },
    T.2.3={ #Event count
      output<-input %>%filter(input[[concept]] == argument) %>%  group_by(CODPACI) %>% summarise(event_count = n())
    })

return(output)
}
