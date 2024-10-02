library(usethis)

#configurar o git
usethis::use_git_config(
  user.name="augustofilho",
  user.email="augustofilho@yahoo.com.br")

#Criando o token
usethis::create_github_token()

#Inserindo o token
usethis::edit_r_environ()

#Avaliando a configuração
usethis::git_sitrep()

#Iniciar o painel do Git
usethis::use_git()

#Adicionar nome e e-mail
edit_git_config()

###Jogar no github
usethis::use_github()

#Como conectar o github da maquina com o Rstudio
library(gitcreds)
gitcreds_set()

