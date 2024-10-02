#Definicao da pasta de trabalho
setwd("/cloud/project/Gabriella_Ferreira")



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
base <- read_excel("base_gabriella3.xlsx",sheet = "Página1")



#Transformando todas as variáveis chr em factor
base |>  dplyr::mutate_if(is.character,as.factor)-> base

#### Retirar as colunas amarelas
base$NOME <- NULL
base$`Data Atend` <- NULL
base$`Lig. 3 dias` <- NULL
base$`Lig. 7 dias` <- NULL
base$`Leito` <- NULL
base$`Prontuário` <- NULL


#### Caracterização dos dados
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(type=list(c(PN,
                          `DOR (EVA)`,
                          SENTAR,
                          LEVANTAR,
                          CAMINHAR,
                          `DOR (EVA)-pos atend`,
                          `SENTAR-pos atend`,
                          `LEVANTAR-pos atend`,
                          `CAMINHAR-pos atend`,
                          `DOR (EVA) - 3dias`,
                          `SENTAR - 7dias`,
                          `LEVANTAR-7dias`,
                          `DOR (EVA) -  7dias`)~"categorical")) |> 
  bold_labels() |> 
  modify_caption("**Caracterização da Amostra**") 




############ Quantitativo
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(type=list(c(`CAMINHAR-7dias`,
                          `LEVANTAR-7dias`,
                          `CAMINHAR- 3dias`,
                          `LEVANTAR - 3dias`,
                          `SENTAR - 3dias`,
                          `LEVANTAR-pos atend`,
                          `SENTAR-pos atend`)~"continuous")) |> 
  bold_labels() |> 
  modify_caption("**Caracterização da Amostra**") 


########## Cruzamento 1 - Dor (EVA)
#### Caracterização dos dados
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(by=`DOR (EVA)`,
              type=list(c(PN,
                          `DOR (EVA)`,
                          SENTAR,
                          LEVANTAR,
                          CAMINHAR,
                          `DOR (EVA)-pos atend`,
                          `SENTAR-pos atend`,
                          `LEVANTAR-pos atend`,
                          `CAMINHAR-pos atend`,
                          `DOR (EVA) - 3dias`,
                          `SENTAR - 7dias`,
                          `LEVANTAR-7dias`,
                          `DOR (EVA) -  7dias`)~"categorical")) |> 
  bold_labels() |> 
  modify_caption("**Caracterização da Amostra**") |> 
  add_p()




########## Cruzamentos por Grupo
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(by=Grupo,
              type=list(c(`CAMINHAR-7dias`,
                          `LEVANTAR-7dias`,
                          `CAMINHAR- 3dias`,
                          `LEVANTAR - 3dias`,
                          `SENTAR - 3dias`,
                          `LEVANTAR-pos atend`,
                          `SENTAR-pos atend`)~"continuous")) |> 
  bold_labels() |> 
  modify_caption("**Caracterização da Amostra**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**GRUPOS**") |>
  add_p() |>
  bold_p() |>
  add_overall(col_label="**Total N**  \nN = {style_number(N)}")


#############################

#### Caracterização dos dados
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(by=Grupo,
              type=list(c(PN,
                          `DOR (EVA)`,
                          SENTAR,
                          LEVANTAR,
                          CAMINHAR,
                          `DOR (EVA)-pos atend`,
                          `SENTAR-pos atend`,
                          `LEVANTAR-pos atend`,
                          `CAMINHAR-pos atend`,
                          `DOR (EVA) - 3dias`,
                          `SENTAR - 7dias`,
                          `LEVANTAR-7dias`,
                          `DOR (EVA) -  7dias`)~"categorical")) |> 
  bold_labels() |> 
  modify_caption("**Caracterização da Amostra**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**GRUPOS**") |>
  add_p() |>
  bold_p() |>
  add_overall(col_label="**Total N**  \nN = {style_number(N)}")


#######
#Fatores relacionados (idade, IMC, paridade (PC +PN), 
#pratica ativ fisica, peso do bebe) com nível de dor EVA, SENTAR, LEVANTAR, CAMINHAR no pós parto imediato:
#11.1 de todas as mulheres
#11.2  entre grupos 1 e 2
















#### Cruzamento ####################################
theme_gtsummary_language(language = "pt")
base |> 
  tbl_summary(by=tonto,
              missing = "no") |> 
  bold_labels() |> 
  modify_caption("**Caracterização da Amostra**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**CRUZAMENTO**") |> 
  add_p() |> 
  bold_p() |>
  add_overall(col_label="**Total N**  \nN = {style_number(N)}")


#################################################
########### GRÁFICO PARA LINKERT
#################################################
base %>% select(Q14:Q18)->graf3
graf3 <- lapply(graf3,function(x){factor(x,levels=c("Nunca", "Quase nunca", "Algumas vezes","Muitas vezes", "Quase sempre"),labels=c(
  "Nunca","Quase nunca","Algumas vezes","Muitas vezes","Quase sempre"))})

#Chamando a biblioteca
lik3 <- likert(as.data.frame(graf3))
plot(lik3,wrap=60,text.size=3)+theme(axis.title.y = element_text(size=10))+
  guides(fill=guide_legend(title="Respostas:"))+
  labs(y="Porcentagem")




# Salvar a tabela em .DOC -------------------------------------------------
library(flextable)
tab16 <- as_flex_table(tab16)
save_as_docx(tab16,path = "C:/Users/augusto.filho/Bioestatistica_R/tabela.docx")



### Gráfico de Funil
apyramid::age_pyramid(data = base,
                      age_group = "Classificação do IMC",
                      split_by = "SEXO",
                      prop = TRUE)+
  theme_classic()


#################################################
##### COM ALTERAÇÃO DE NOMES:
# Renomeando as colunas
base <- base %>% 
  rename(
    Gravidez = Classe_G,
    Parto_Cesaria = Classe_PC,
    Parto_Normal = Classe_PN,
    Aborto = Classe_A
  )

# Selecionando as colunas desejadas
base %>% 
  select(Gravidez,
         Parto_Cesaria,
         Parto_Normal,
         Aborto) -> graf3

# Convertendo as colunas para fatores
graf3 <- lapply(graf3,function(x){factor(x,levels=c("Não","Sim"),
                                         labels=c("NÃO","SIM"))})

# Criando o gráfico de Likert
lik3 <- likert(as.data.frame(graf3))
plot(lik3,wrap=60,text.size=3)+theme(axis.title.y = element_text(size=10))+
  guides(fill=guide_legend(title="Respostas:"))+
  labs(y="Porcentagem")



############ SEGUNDO PEDIDO
base %>% 
  dplyr::select(`Idade`,
                `IMC`,
                 G,
                `Pratica atv física`,
                `Peso RN 1 (g)`,
                `Tipo de parto`,
                `DOR (EVA)`,
                `SENTAR`,
                LEVANTAR,
                CAMINHAR)-> base_matriz

matriz=round(cor(base_matriz[1:10],method="kendall"),2)
corrplot(matriz,method = "circle",
         type="upper",order="hclust",
         addCoef.col = "black",
         tl.col="black",tl.srt=45,
         diag=FALSE)





# Transformando todas as variáveis chr em factor
base <- base |> dplyr::mutate(across(where(is.character), as.factor))

# Remover colunas indesejadas
#base <- base |> 
  #dplyr::select(-NOME, -`Data Atend`, -`Lig. 3 dias`, -`Lig. 7 dias`, -Leito, -Prontuário)

# Segundo pedido: seleção das colunas de interesse
base_matriz <- base %>%
  dplyr::select(`Idade`, `IMC`, G, `Peso RN 1 (g)`, 
                 `DOR (EVA)`, `SENTAR`, LEVANTAR, CAMINHAR)

# Remover NAs
base_matriz <- na.omit(base_matriz)

# Remover variáveis constantes
base_matriz <- base_matriz[, sapply(base_matriz, function(x) length(unique(x)) > 1)]

# Calcular a matriz de correlação
matriz <- round(cor(base_matriz, method = "kendall"), 2)

# Substituir valores NA/NaN/Inf por zero (ou outra estratégia, se preferir)
matriz[is.na(matriz)] <- 0

# Corrplot
corrplot(matriz, method = "circle", type = "upper", order = "hclust",
         addCoef.col = "black", tl.col = "black", tl.srt = 45, diag = FALSE)



########### Teste
#### Caracterização dos dados
theme_gtsummary_language(language = "pt")
base |> 
  dplyr::select(`TIPO DE PARTO2`,
                Idade,
                IMC,
                G,
                `Pratica atv física`,
                `Peso RN 1 (g)`,
                `DOR (EVA)`,
                `SENTAR`,
                LEVANTAR,
                CAMINHAR) %>% 
  tbl_summary(by=`TIPO DE PARTO2`) %>% 
  bold_labels() |> 
  modify_caption("**Caracterização da Amostra**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**TIPO DE PARTO**") |>
  add_p() |>
  bold_p() |>
  add_overall(col_label="**Total N**  \nN = {style_number(N)}")





#### Caracterização dos dados
theme_gtsummary_language(language = "pt")
base |> 
  dplyr::select(`TIPO DE PARTO2`,
                Idade,
                IMC,
                G,
                `Pratica atv física`,
                `Peso RN 1 (g)`,
                `DOR (EVA)`,
                `SENTAR`,
                LEVANTAR,
                CAMINHAR) %>% 
  tbl_summary(by=`Pratica atv física`) %>% 
  bold_labels() |> 
  modify_caption("**Caracterização da Amostra**") |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Pratica Atividade Física**") |>
  add_p() |>
  bold_p() |>
  add_overall(col_label="**Total N**  \nN = {style_number(N)}")

###################################