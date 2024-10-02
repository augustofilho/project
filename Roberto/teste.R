#Definicao da pasta de trabalho
setwd("/cloud/project/Roberto")


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
               rstatix)




#Carregando base de dados
base <- read_excel("base_augusto.xlsx",sheet = "base")


#Ordenação das respostas da variável idade
base$idade <- factor(base$idade, 
                     levels = c("Menores de 1 ano",
                                "1 ano",
                                "2 anos",
                                "3 anos",
                                "4 anos",
                                "5 anos",
                                "6 anos"))



#Transformando todas as variáveis chr em factor
base |>  dplyr::mutate_if(is.character,as.factor)-> base



#### Caracterização dos dados
theme_gtsummary_language(language = "pt")
base |>
  tbl_summary(missing = "no", 
              type=list(`motocicletas`~"categorical")) |> 
  bold_labels() |> 
  modify_caption("**Caracterização da Amostra**") 





#### Cruzamento ####################################
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(by=Grupo,
              missing = "no") |> 
  bold_labels() |> 
  modify_caption("**Caracterização da Amostra**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**GRUPO**") |> 
  add_overall(col_label="**Total N**  \nN = {style_number(N)}")



#################################################
########### GRÁFICO PARA LINKERT
#################################################
base %>% select(`Classe Dominio 1`,
                `Classe Dominio 2`,
                `Classe Dominio 3`,
                `Classe Dominio 4`)->graf3
graf3 <- lapply(graf3,function(x){factor(x,
                                         levels=c("Necessita melhorar",
                                                  "Regular",
                                                  "Boa",
                                                  "Muito Boa"),
                                         labels=c("Necessita melhorar",
                                                  "Regular",
                                                  "Boa",
                                                  "Muito Boa"))})

#Chamando a biblioteca
lik3 <- likert(as.data.frame(graf3),grouping = base$Grupo)
plot(lik3,wrap=60,text.size=3)+theme(axis.title.y = element_text(size=10))+
  guides(fill=guide_legend(title="Respostas:"))+
  labs(y="Porcentagem")



########### ENCONTRO DO DIA 01/10/2024
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(by=`Classificação ABEP`,
              missing = "no") |> 
  bold_labels() |> 
  modify_caption("**Caracterização da Amostra para ABEP**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**ABEP**") |> 
  add_overall(col_label="**Total N**  \nN = {style_number(N)}") %>% 
  add_p() %>% 
  bold_p()



######### SEGUNDA VERSÃO ##################
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(by=`Classificação ABEP-2`,
              missing = "no") |> 
  bold_labels() |> 
  modify_caption("**Caracterização da Amostra para ABEP**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**ABEP**") |> 
  add_overall(col_label="**Total N**  \nN = {style_number(N)}") %>% 
  add_p() %>% 
  bold_p()



