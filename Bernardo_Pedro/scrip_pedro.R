#Definicao da pasta de trabalho
#setwd("C:/Users/augusto.filho/Bioestatistica_R/Pedro_Figueiredo&Bernando_Nunes")
setwd("/cloud/project/Bernardo_Pedro")



#Carregamento das principais bibliotecas
if(!require(pacman)) install.packages("pacman")
library(pacman)

# Usando o package pacman para carregar as bibliotecas
pacman::p_load(dplyr,
               readxl,
               scales,
               ggplot2,
               forcats,
               psych,
               gtsummary,
               broom,
               gt,
               labelled,
               esquisse,
               sjPlot,
               likert,
               ggstatsplot,
               apyramid,
               tm,
               ggiraph,
               patchwork,
               purrr)


#Carregando base de dados
base <- read_excel("base_pedro_3.xlsx")







##### Não carregue as colunas
base$`Código` <- NULL
base$`Inicias` <- NULL
base$`cT_local` <- NULL
base$`Cir_data` <- NULL
base$`Comments` <- NULL
base$`outros_diag` <- NULL
base$`inf_pele` <- NULL
base$`Nivel_inva` <- NULL
base$`eosino` <- NULL
base$`Coilo` <- NULL
base$`Quant_IE` <- NULL
base$`Quant_IE_pos` <- NULL
base$`Ext_IE` <- NULL
base$`Quant_ID` <- NULL
base$`Quant_ID_pos` <- NULL
base$`Ext_ID` <- NULL
base$`pTT` <- NULL
base$`pT_1b` <- NULL
base$`pNN` <- NULL
base$`Estadio` <- NULL
base$`Data_obito` <- NULL

#Ordenar as respostas da variável Classe_Meses
base$Classe_Meses <- factor(base$Classe_Meses, 
                            levels = c("Até 12 meses",
                                       "Acima de 12 meses"))


#### Ordenar as respostas da variável Classe de idades
base$`Classe de idades`<-factor(base$`Classe de idades`,
                                levels = c("<35",
                                           "35-49",
                                           "50-64",
                                           "65-79",
                                           ">80"))

base$`Classe_tempo_de_dreno`<-factor(base$`Classe_tempo_de_dreno`,
                                   levels = c("0-3 dias",
                                              "4-6 dias",
                                              ">6 dias"))

base$`TT_mm3_classes`<-factor(base$`TT_mm3_classes`,
                              levels = c("< 25 mm3",
                                      "25 - 100 mm3",
                                      ">100 mm3"))



#Transformando todas as variáveis chr em factor
base |>  dplyr::mutate_if(is.character,as.factor)-> base


#### Caracterização dos dados
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(missing = "no",
              type=list(c(`Fimose`,`Tabagista`)~"categorical")) |> 
  bold_labels() |> 
  modify_caption("**Caracterização da Amostra**") 



###### Exemplo de caracterização
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(by=Classe_Meses,
              missing = "no",
              type=list(c(`Fimose`,`Tabagista`)~"categorical")) |> 
  bold_labels() |> 
  modify_caption("**Caracterização da Amostra por Meses**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Intervalo de Tempo**") 





###### Cruzamento
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(by=Classe_Meses,
              missing = "no",
              type=list(c(`Fimose`,`Tabagista`)~"categorical")) |> 
  bold_labels() |> 
  modify_caption("**Caracterização da Amostra por Meses**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Intervalo de Tempo**") |> 
  add_p() |>
  bold_p() |>
  add_overall(col_label="**Total N**  \nN = {style_number(N)}")





###### Cruzamento NOVA PARA O SIEES - Cruzamento 1
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(by=Linfadec,
              missing = "no") %>% 
  bold_labels() |> 
  modify_caption("**Cruzamento de Classe Linfadec com demais variáveis**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Classe Linfadec**") |> 
  add_p() |>
  bold_p() |>
  add_overall(col_label="**Total N**  \nN = {style_number(N)}")


##### Cruzamento 2
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(by=`Classe de idades`,
              missing = "no") %>% 
  bold_labels() |> 
  modify_caption("**Cruzamento de Classe de Idades com demais variáveis**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Classe de idades**") |> 
  add_p() |>
  bold_p() |>
  add_overall(col_label="**Total N**  \nN = {style_number(N)}")



############### Cruzamento 3
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(by=`Classe_Meses`,
              missing = "no") %>% 
  bold_labels() |> 
  modify_caption("**Cruzamento de Classe Meses com demais variáveis**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Classe_Meses**") |> 
  add_p() |>
  bold_p() |>
  add_overall(col_label="**Total N**  \nN = {style_number(N)}")



############### Cruzamento 4
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(by=`Fimose`,
              missing = "no") %>% 
  bold_labels() |> 
  modify_caption("**Cruzamento de Fimose com demais variáveis**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Fimose**") |> 
  add_p() |>
  bold_p() |>
  add_overall(col_label="**Total N**  \nN = {style_number(N)}")





############### Cruzamento 5
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(by=`Tabagista`,
              missing = "no") %>% 
  bold_labels() |> 
  modify_caption("**Cruzamento de Tabagista com demais variáveis**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Tabagista**") |> 
  add_p() |>
  bold_p() |>
  add_overall(col_label="**Total N**  \nN = {style_number(N)}")





############### Cruzamento 6
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(by=`Estado_Civil`,
              missing = "no") %>% 
  bold_labels() |> 
  modify_caption("**Cruzamento de Estado_Civil com demais variáveis**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Estado_Civil**") |> 
  add_p() |>
  bold_p() |>
  add_overall(col_label="**Total N**  \nN = {style_number(N)}")




############### Cruzamento 7
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(by=`cT`,
              missing = "no") %>% 
  bold_labels() |> 
  modify_caption("**Cruzamento de cT com demais variáveis**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**cT**") |> 
  add_p() |>
  bold_p() |>
  add_overall(col_label="**Total N**  \nN = {style_number(N)}")




############### Cruzamento 8
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(by=`cT`,
              missing = "no") %>% 
  bold_labels() |> 
  modify_caption("**Cruzamento de cT com demais variáveis**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**cT**") |> 
  add_p() |>
  bold_p() |>
  add_overall(col_label="**Total N**  \nN = {style_number(N)}")


############### Cruzamento 9
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(by=`cN`,
              missing = "no") %>% 
  bold_labels() |> 
  modify_caption("**Cruzamento de cN com demais variáveis**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**cN**") |> 
  add_p() |>
  bold_p() |>
  add_overall(col_label="**Total N**  \nN = {style_number(N)}")




############### Cruzamento 10
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(by=`cN`,
              missing = "no") %>% 
  bold_labels() |> 
  modify_caption("**Cruzamento de cN com demais variáveis**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**cN**") |> 
  add_p() |>
  bold_p() |>
  add_overall(col_label="**Total N**  \nN = {style_number(N)}")
