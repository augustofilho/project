#Definicao da pasta de trabalho
setwd("/cloud/project/Matheus")


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
               purrr, 
               rstatix,
               corrplot)


#Carregando base de dados
base <- read_excel("base_Matheus.xlsx",sheet = "base")


##### Não rode as colunas
base$`Carimbo de data/hora` <- NULL
base$`Nome:` <- NULL
base$`Data de nascimento:`<- NULL
base$`Data da Lesão:` <- NULL
base$`Data da RM:` <- NULL
base$`Data do Exame Físico:` <- NULL
base$`Força isométrica 30° - D`<- NULL
base$`Força isométrica 45° - D` <- NULL
base$`Força isométrica 60° - D` <- NULL
base$`Força isométrica 30° - E` <- NULL
base$`Força isométrica 45° - E` <- NULL
base$`Força isométrica 60° - E` <- NULL


#Transformando todas as variáveis chr em factor
base |>  dplyr::mutate_if(is.character,as.factor)-> base






#### Caracterização dos dados
theme_gtsummary_language(language = "pt")
base |>
  tbl_summary(type=list(c(`Idade:`,
                          `Força isométrica 30° - Lado Acometido`,
                          `Força isométrica 45° - Lado Acometido`,
                          `Força isométrica 60° - Lado Acometido`,
                          `Força isométrica 30° - Lado Não acometido`,
                          `Força isométrica 45° - Lado Não acometido`,
                          `Força isométrica 60° - Lado Não acometido`,
                          `Peso (Kg)`,
                          `Altura (m)`,
                          `Alavanca (cm)`)~"continuous")) %>% 
  bold_labels()
  


########### CRUZAMENTOS - objetivo
theme_gtsummary_language(language = "pt")
base |>
  dplyr::select(`Se sim, especifique o grau da lesão:`,
                `% Perda de força 30 grau`,
                `% Perda de força 45 grau`,
                `% Perda de força 60 grau`) %>% 
  tbl_summary(by=`Se sim, especifique o grau da lesão:`,
              type=list(c(`% Perda de força 30 grau`,
                          `% Perda de força 45 grau`,
                          `% Perda de força 60 grau`)~"continuous")) %>%
  add_p() %>% 
  bold_p() %>% 
  add_overall(col_label="**Total N**  \nN = {style_number(N)}")
  
  

######## 
#### Caracterização dos dados
theme_gtsummary_language(language = "pt")
base |>
  tbl_summary(by=`Se sim, especifique o grau da lesão:`,
                type=list(c(`Idade:`,
                          `Força isométrica 30° - Lado Acometido`,
                          `Força isométrica 45° - Lado Acometido`,
                          `Força isométrica 60° - Lado Acometido`,
                          `Força isométrica 30° - Lado Não acometido`,
                          `Força isométrica 45° - Lado Não acometido`,
                          `Força isométrica 60° - Lado Não acometido`,
                          `Peso (Kg)`,
                          `Altura (m)`,
                          `Alavanca (cm)`)~"continuous")) %>% 
  bold_labels()


