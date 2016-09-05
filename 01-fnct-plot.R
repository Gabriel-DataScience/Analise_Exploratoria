
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# função que retorna os dados e um auxiliar
dados <- function(input){
#- Reactive Dataset Generation -#
# essa função é usada para os dados dos exemplos
  datasetInput <- reactive({
    switch(input$ExemplodeDados,
           "cars" = cars,
           "faithful" = faithful,
           "iris" = iris,
           "mtcars" = mtcars)
  })
  
  if(req(input$Escolha) == "Exemplo") dados <- datasetInput()

  if(input$Escolha == "Seu própio banco de dados"){
    inFile <- req(input$arquivoescolhido)
    dados <- read.csv(inFile$datapath, header = input$header,
                      sep = input$sep, dec = input$dec, encoding = "UTF-8")
  }
  return(list(dados=dados))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# essa função vai retornar as variáveis que são númericas
# e também o banco de dados transformado em factor as colunas com o nome
# que a pessoa marcou em input.SelecionarQualitativa
Verificar_Variaveis <- function(input){
  dados <- dados(input)$dados
  
  Verificar_numeric0000 <- sapply(dados, is.numeric)
  Colunas_numeric0000 <- which(Verificar_numeric0000 == TRUE)
  
  if(!is.null(input$SelecionarQualitativa)){
    for(i in input$SelecionarQualitativa){
      coluna <- which(colnames(dados) == i)
      dados[,coluna] <- as.factor(dados[,coluna])
    }
  }
  dados <- as.data.frame(dados)

  if(length(input$SelecionarQualitativa) >= length(Colunas_numeric0000)) Colunas_numeric <- 0
  else{
    Verificar_numeric <- sapply(dados, is.numeric)
    Colunas_numeric <- which(Verificar_numeric == TRUE)
  }

  return(list(Colunas_numericas = Colunas_numeric, Dados = dados))
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# função que retorna uma matriz com as estatísticas
Sumario <- function(x){
  sumario <- matrix(nrow = 16, ncol = ncol(x))
  for(i in 1:ncol(x)){
    sumario[,i] <- basicStats(x[,i])[[1]]
  }
  rownames(sumario) <- c("Nº obs", "NAs", "Mínimo", "Máximo", "1. quartil",
                         "3. quartil", "Média", "Mediana", "Somatório", "EP média",
                         "IC_inf Média", "IC_sup Média", "Variância", "Desvio padrão",
                         "Assimetria", "Curtose")
  colnames(sumario) <- colnames(x)
  return(sumario)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#



