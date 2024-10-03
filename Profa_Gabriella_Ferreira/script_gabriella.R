#Definicao da pasta de trabalho
setwd("/cloud/project/Profa_Gabriella_Ferreira")



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




#### Retirar as colunas amarelas
base$NOME <- NULL
base$`Data Atend` <- NULL
base$`Lig. 3 dias` <- NULL
base$`Lig. 7 dias` <- NULL
base$`Leito` <- NULL
base$`Prontuário` <- NULL

#Transformando todas as variáveis chr em factor
base |>  dplyr::mutate_if(is.character,as.factor)-> base



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

################################################################
######### SEGUNDA PARTE DO PEDIDO - REUNIÃO DO DIA 01/10/2024
################################################################


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




#########################################
#########################################
#########################################
############ SEGUNDO PEDIDO #############
#########################################

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


#### Testando a normalidade dos dados
resultados_shapiro <- list(
  "SENTAR" = shapiro.test(base$SENTAR),
  "LEVANTAR"=shapiro.test(base$LEVANTAR),
  "CAMINHAR" =shapiro.test(base$CAMINHAR),
  "IMC"=shapiro.test(base$IMC),
  "DOR (EVA)" =shapiro.test(base$`DOR (EVA)`),
  "Peso RN 1 (g)" = shapiro.test(base$`Peso RN 1 (g)`),
  "Idade" = shapiro.test(base$Idade)
)

#Extrair as estatísticas e valores-p dos resultados do teste
shapiro_resumo <- resultados_shapiro %>%
  map_df(~ data.frame(Estatística_W = .x$statistic, Valor_p = .x$p.value), .id = "Variável")

# Criar a tabela formatada com o pacote gt
tabela_shapiro <- shapiro_resumo %>%
  gt() %>%
  tab_header(
    title = md("**Resultados do Teste de Normalidade (Shapiro-Wilk)**"),
    subtitle = md("Análise das variáveis numéricas da base de dados")
  ) %>%
  fmt_number(
    columns = vars(Estatística_W, Valor_p),
    decimals = 4
  ) %>%
  tab_source_note(md("**Obs:** Teste de normalidade realizado com base no teste Shapiro-Wilk.")) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

# Exibir a tabela
tabela_shapiro


##### Teste de Correlação para a matriz de correlação

base <- read_excel("base_gabriella3.xlsx",sheet = "Página1")
base_matriz<-base %>% 
  select(`Idade`, 
         `IMC`, 
          G,
         `Peso RN 1 (g)`,
         `DOR (EVA)`, 
         `SENTAR`,
         LEVANTAR, 
         CAMINHAR)

#Função para realizar o teste de correlação de Kendall
realiza_teste_correlacao <- function(df, var1_name, var2_name) {
  teste <- cor.test(df[[var1_name]], df[[var2_name]], method = "spearman")
  return(data.frame(
    Variável_1 = var1_name,
    Variável_2 = var2_name,
    Coeficiente = round(teste$estimate, 3),
    Valor_p = round(teste$p.value, 4)
  ))
}


# Realizar o teste de correlação entre pares de variáveis
resultados_correlacao <- bind_rows(
  realiza_teste_correlacao(base_matriz,"G","SENTAR"),
  realiza_teste_correlacao(base_matriz,"G","LEVANTAR"),
  realiza_teste_correlacao(base_matriz,"G","IMC"),
  realiza_teste_correlacao(base_matriz,"G","DOR (EVA)"),
  realiza_teste_correlacao(base_matriz,"G","Peso RN 1 (g)"),
  realiza_teste_correlacao(base_matriz,"G","Idade"),
  
  realiza_teste_correlacao(base_matriz,"Idade","SENTAR"),
  realiza_teste_correlacao(base_matriz,"Idade","LEVANTAR"),
    realiza_teste_correlacao(base_matriz,"Idade","CAMINHAR"),
  realiza_teste_correlacao(base_matriz,"Idade","IMC"),
  realiza_teste_correlacao(base_matriz,"Idade","DOR (EVA)"),
  realiza_teste_correlacao(base_matriz,"Idade","Peso RN 1 (g)"),
                           
  realiza_teste_correlacao(base_matriz,"Peso RN 1 (g)","SENTAR"),
  realiza_teste_correlacao(base_matriz,"Peso RN 1 (g)","LEVANTAR"),
  realiza_teste_correlacao(base_matriz,"Peso RN 1 (g)","CAMINHAR"),
  realiza_teste_correlacao(base_matriz,"Peso RN 1 (g)","IMC"),
  realiza_teste_correlacao(base_matriz,"Peso RN 1 (g)","DOR (EVA)"),
  
  realiza_teste_correlacao(base_matriz,"DOR (EVA)","SENTAR"),
  realiza_teste_correlacao(base_matriz,"DOR (EVA)","LEVANTAR"),
  realiza_teste_correlacao(base_matriz,"DOR (EVA)","CAMINHAR"),
  realiza_teste_correlacao(base_matriz,"DOR (EVA)","IMC"),
  
  realiza_teste_correlacao(base_matriz,"IMC","SENTAR"),
  realiza_teste_correlacao(base_matriz,"IMC","LEVANTAR"),
  realiza_teste_correlacao(base_matriz,"IMC","CAMINHAR"),
  
  realiza_teste_correlacao(base_matriz,"CAMINHAR","SENTAR"),
  realiza_teste_correlacao(base_matriz,"CAMINHAR","LEVANTAR"),
  
)
                           
   
# Criar a tabela usando gt()
tabela_correlacao <- resultados_correlacao %>%
  gt() %>%
  tab_header(
    title = md("**Resultados do Teste de Correlação de Spearman**"),
    subtitle = md("Correlação entre Obitos e variáveis selecionadas")
  ) %>%
  # Formatar o valor-p com "*" para valores menores que 0,05
  fmt(
    columns = c(Valor_p),
    fns = function(x) {
      ifelse(x < 0.05, sprintf("%.4f*", x), sprintf("%.4f", x))
    }
  ) %>%
  # Formatar o coeficiente de correlação com 3 casas decimais
  fmt_number(
    columns = c(Coeficiente),
    decimals = 3
  ) %>%
  # Estilizar o cabeçalho
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

# Exibir a tabela
tabela_correlacao

