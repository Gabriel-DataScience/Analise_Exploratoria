#----------------------------- Atividade Final -------------------------------#
#------------------------ Estatística computacional --------------------------#
#--------------------------- Lissandra e Gabriel -----------------------------#
source("00-init.R", encoding = "UTF-8")
source("02-ui_elements.R", encoding = "UTF-8")
source("01-fnct-plot.R", encoding = "UTF-8")
# Início Pagina do Usuário
#------------------------------------------------------------------------------#

ui <- 
  dashboardPage( skin = "red",
    dashboardHeader(title = "Estatistica"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Tutorial", tabName = "Tutorial", icon = icon("fa fa-book")),
        menuItem("Dados", tabName = "Dados", icon = icon("fa fa-usb")),
        menuItem("Descritiva", tabName = "Descritiva", icon = icon("fa fa-list-alt")),
        menuItem("Gráficos Univariados", tabName = "Graficos", icon = icon("fa fa-bar-chart")),
        menuItem("Gráficos Bivariados", tabName = "Graficos_Bi", icon = icon("fa fa-bar-chart"))
        
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "Tutorial",
                titlePanel("Tutorial para abrir o banco de dados"),
                paste("  Esse é o Tutorial para abrir o banco de dados")
        ),
#------------------------------------------------------------------------------#
        tabItem(tabName = "Dados",
              fluidRow(
                box( title = "Escolha dos dados", solidHeader = TRUE, status = "danger", width = 4,
                     Ex_ou_enter, hr(),
                  conditionalPanel( condition = "input.Escolha == 'Exemplo'",
                                    enter_exemplo
                  ), hr(),
                  conditionalPanel( condition = "input.Escolha == 'Seu própio banco de dados'",
                                    enter_dados,
                                    Header,
                                    Separador,
                                    Decimal
                  ),
                  uiOutput("nomes_das_colunas2")
                ),
                mainPanel(dataTableOutput("Dados"))
              )
        ),
#------------------------------------------------------------------------------#
        tabItem(tabName = "Descritiva",
                sidebarLayout(
                  box( title = "Variáveis qualitativas", solidHeader = TRUE, status = "danger",
                    uiOutput("nomes_das_colunas")
                  ),
                  mainPanel( width = 12,
                    tabBox( title = tagList(shiny::icon("gear"), "Tipo de variável"),
                          side = "left", width = 10000,
                      tabPanel(
                       "Variáveis quantitativas",tableOutput("TabelaDescritiva_quantitativa") 
                      ),
                      tabPanel(
                       "Variáveis qualitativas",tableOutput("TabelaDescritiva_qualitativa")
                      )                     
                    )
                  )

                )
        ),
#------------------------------------------------------------------------------#
        tabItem(tabName = "Graficos",
                fluidRow(
                  column(width=4,
                          box(title = "Escolha do Gráfico", solidHeader = TRUE, status = "danger",
                            width = NULL,
                            TipoGrafico,
                            conditionalPanel( 
                              condition = 
                                "input.tipo == 'Colunas' | 
                                input.tipo == 'Barras' |
                                input.tipo == 'Pizza' ",
                                  uiOutput("nomes_das_colunas_quali")
                            ), 
                            conditionalPanel(
                              condition = "input.tipo == 'Histograma'",
                                uiOutput("nomes_das_colunas_quanti")
                            ),
                            conditionalPanel(
                              condition = "input.tipo == 'Linhas'",
                                uiOutput("nomes_das_colunas3"))
                          ),
                          box(title = "Formatação", solidHeader = TRUE, status = "danger",
                            width = NULL,
                            InserirTitulo,
                            InserirEixo,
                            conditionalPanel(
                              condition = "input.tipo == 'Histograma'",
                              NumeroClasses
                            )
                          )
                  ),
                  column(8, 
                         box(width = NULL, status = "danger" ,plotlyOutput("grafico"))
                  )
                )
        ),
        tabItem(tabName = "Graficos_Bi",
                fluidRow(
                  column(width=4,
                         box(title = "Escolha do Gráfico", solidHeader = TRUE, status = "danger",
                             width = NULL,
                             TipoGrafico_Bi,
                             conditionalPanel( 
                               condition = 
                               "input.tipo_Bi == 'Colunas' |
                               input.tipo_Bi == 'Colunas2' |
                               input.tipo_Bi == 'Colunas3' |
                               input.tipo_Bi == 'Barras'|
                               input.tipo_Bi == 'Barras2'|
                               input.tipo_Bi == 'Barras3'",
                               uiOutput("nomes_das_colunas_quali_Bi"),
                               uiOutput("nomes_das_colunas_quali_Bi2")
                             ), 
                             conditionalPanel(
                               condition = "input.tipo_Bi == 'Pontos'",
                               uiOutput("nomes_das_colunas_quanti_Bi"),
                               uiOutput("nomes_das_colunas_quanti_Bi2")
                             ),
                             conditionalPanel(
                               condition = "input.tipo_Bi == 'Linhas'|
                               input.tipo_Bi == 'Histograma' |
                               input.tipo_Bi == 'BoxPlot'",
                               uiOutput("nomes_das_colunas3_Bi"),
                             uiOutput("nomes_das_colunas3_Bi2"))
                         ),
                         box(title = "Formatação", solidHeader = TRUE, status = "danger",
                             width = NULL,
                             InserirTitulo_Bi,
                             InserirEixo_Bi,
                             conditionalPanel(
                               condition = "input.tipo_Bi == 'Histograma'",
                               NumeroClasses_Bi
                             )
                         )
                         ),
                  column(8, 
                         box(width = NULL, status = "danger" ,plotlyOutput("grafico_Bi"))
                  )
                )
          
        )

      )
      
      
    )
  )
#------------------------------------------------------------------------------#
# Final do layout
      



# Início Server
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

server <- function(input, output, session)
{
  
#- output: renderUI - nomes das colunas- - - - - - - - - - - - - - - - - - - - #

# nome das colunas para selecionar as qualitativas
  output$nomes_das_colunas <- renderUI({
    dados <- dados(input)
    if(dados$aux == 1)  nomes <- as.matrix("Escolha o arquivo com o banco de dados")
    else nomes <- as.matrix(colnames(dados$dados))
    
    UIselectqualitativa(nomes)
  })
  
# função para a seleção das colunas a visualizar do banco de dados
  output$nomes_das_colunas2 <- renderUI({
    dados <- dados(input)
    if(dados$aux == 1)  nomes <- as.matrix("Escolha o arquivo com o banco de dados")
    else nomes <- as.matrix(colnames(dados$dados))
    
    UIvervariaveis(nomes)
  })

# Nome das colunas para fazer o gráfico de linhas
  output$nomes_das_colunas3 <- renderUI({
    dados <- dados(input)
    if(dados$aux == 1)  nomes <- as.matrix("Escolha o arquivo com o banco de dados")
    else nomes <- as.matrix(colnames(dados$dados))
    
    UIvarlinhas(nomes)
  })
# Nome das colunas para fazer o gráfico de linhas bivariado
  output$nomes_das_colunas3_Bi <- renderUI({
    dados <- dados(input)
    colunas_numericas <- Verificar_Variaveis(input)$Colunas_numericas
    
    if(dados$aux == 1)  nomes <- as.matrix("Escolha o arquivo com o banco de dados")
    else{
      if(length(colunas_numericas) == ncol(dados$dados))
        nomes <- as.matrix(colnames(dados$dados))
      else{
        if(colunas_numericas[1] == 0)
          nomes <- as.matrix("")
        else
          nomes <- as.matrix(colnames(dados$dados[colunas_numericas]))
      }
    }
    
    UIvarlinhas_Bi(nomes)
  })
  output$nomes_das_colunas3_Bi2 <- renderUI({
    dados <- dados(input)
    colunas_numericas <- Verificar_Variaveis(input)$Colunas_numericas
    
    if(dados$aux == 1)  nomes <- as.matrix("Escolha o arquivo com o banco de dados")
    else{
      if(colunas_numericas[1] == 0)
        nomes <- as.matrix(colnames(dados$dados))
      else{
        if(length(colunas_numericas) == ncol(dados$dados))
          nomes <- as.matrix("")
        else
          nomes <- as.matrix(colnames(dados$dados[-colunas_numericas]))
      }
    }
    
    UIvarlinhas_Bi2(nomes)
  })
# função para retornar o nome das colunas qualitativas para o gráfico
  output$nomes_das_colunas_quali <- renderUI({
    dados <- dados(input)
    colunas_numericas <- Verificar_Variaveis(input)$Colunas_numericas
    
    if(dados$aux == 1)  nomes <- as.matrix("Escolha o arquivo com o banco de dados")
    else{
      if(colunas_numericas[1] == 0)
        nomes <- as.matrix(colnames(dados$dados))
      else{
        if(length(colunas_numericas) == ncol(dados$dados))
          nomes <- as.matrix("")
        else
          nomes <- as.matrix(colnames(dados$dados[-colunas_numericas]))
      }
    }
    UIvarquali(nomes)
  })
  
  # função para retornar o nome das colunas qualitativas para o gráfico bivariado
  output$nomes_das_colunas_quali_Bi <- renderUI({
    dados <- dados(input)
    colunas_numericas <- Verificar_Variaveis(input)$Colunas_numericas
    
    if(dados$aux == 1)  nomes <- as.matrix("Escolha o arquivo com o banco de dados")
    else{
      if(colunas_numericas[1] == 0)
        nomes <- as.matrix(colnames(dados$dados))
      else{
        if(length(colunas_numericas) == ncol(dados$dados))
          nomes <- as.matrix("")
        else
          nomes <- as.matrix(colnames(dados$dados[-colunas_numericas]))
      }
    }
    UIvarquali_Bi(nomes)
  })
  
  output$nomes_das_colunas_quali_Bi2 <- renderUI({
    dados <- dados(input)
    colunas_numericas <- Verificar_Variaveis(input)$Colunas_numericas
    
    if(dados$aux == 1)  nomes <- as.matrix("Escolha o arquivo com o banco de dados")
    else{
      if(colunas_numericas[1] == 0)
        nomes <- as.matrix(colnames(dados$dados))
      else{
        if(length(colunas_numericas) == ncol(dados$dados))
          nomes <- as.matrix("")
        else
          nomes <- as.matrix(colnames(dados$dados[-colunas_numericas]))
      }
    }
    UIvarquali_Bi2(nomes)
  })
  
# função para retornar o nome das colunas quantitativas para o gráfico
  output$nomes_das_colunas_quanti <- renderUI({
    dados <- dados(input)
    colunas_numericas <- Verificar_Variaveis(input)$Colunas_numericas
    
    if(dados$aux == 1)  nomes <- as.matrix("Escolha o arquivo com o banco de dados")
    else{
      if(length(colunas_numericas) == ncol(dados$dados))
        nomes <- as.matrix(colnames(dados$dados))
      else{
        if(colunas_numericas[1] == 0)
          nomes <- as.matrix("")
        else
          nomes <- as.matrix(colnames(dados$dados[colunas_numericas]))
      }
    }
    UIvarquanti(nomes)
  })

# função para retornar o nome das colunas quantitativas para o gráfico bivariado
  output$nomes_das_colunas_quanti_Bi <- renderUI({
    dados <- dados(input)
    colunas_numericas <- Verificar_Variaveis(input)$Colunas_numericas
    
    if(dados$aux == 1)  nomes <- as.matrix("Escolha o arquivo com o banco de dados")
    else{
      if(length(colunas_numericas) == ncol(dados$dados))
        nomes <- as.matrix(colnames(dados$dados))
      else{
        if(colunas_numericas[1] == 0)
          nomes <- as.matrix("")
        else
          nomes <- as.matrix(colnames(dados$dados[colunas_numericas]))
      }
    }
    UIvarquanti_Bi(nomes)
  })
  output$nomes_das_colunas_quanti_Bi2 <- renderUI({
    dados <- dados(input)
    colunas_numericas <- Verificar_Variaveis(input)$Colunas_numericas
    
    if(dados$aux == 1)  nomes <- as.matrix("Escolha o arquivo com o banco de dados")
    else{
      if(length(colunas_numericas) == ncol(dados$dados))
        nomes <- as.matrix(colnames(dados$dados))
      else{
        if(colunas_numericas[1] == 0)
          nomes <- as.matrix("")
        else
          nomes <- as.matrix(colnames(dados$dados[colunas_numericas]))
      }
    }
    UIvarquanti_Bi2(nomes)
  })
  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#- output: tabelas dos dados, frequencia - - - - - - - - - - - - - - - - - - - #
  
# Tabela com os dados
  output$Dados <- renderDataTable({
    dados <- dados(input)
    if(dados$aux == 1) as.matrix("Escolha o arquivo com o banco de dados")
    else as.matrix(dados$dados[,input$show_vars, drop = FALSE])
  }, options = list(pageLength = 10)
  )

# table da tabela com as variáveis quantitativas ( númericas )
  output$TabelaDescritiva_quantitativa <- renderTable({
    dados <- dados(input)
    if(dados$aux == 1) as.matrix("Escolha o arquivo com o banco de dados")
    else{
      Colunas_numeric <- Verificar_Variaveis(input)$Colunas_numericas
      dados <- Verificar_Variaveis(input)$Dados
      if(length(Colunas_numeric) == 1){
        if(Colunas_numeric == 0) as.matrix("") else
          Sumario(dados[Colunas_numeric])
      }else
        Sumario(dados[Colunas_numeric])  
    }
  })

# table da tabela com as variáveis qualitativas ( fatores )
  output$TabelaDescritiva_qualitativa <- renderTable({
    dados <- dados(input)
    if(dados$aux == 1) as.matrix("Escolha o arquivo com o banco de dados")
    else{
      Colunas_numeric <- Verificar_Variaveis(input)$Colunas_numericas
      dados <- Verificar_Variaveis(input)$Dados
      if(length(Colunas_numeric) == ncol(dados) ) as.matrix("") else{
        if(length(Colunas_numeric) == 1){
          if(Colunas_numeric == 0) summary(dados)else
            summary(dados[-Colunas_numeric])
        }else
          summary(dados[-Colunas_numeric])
      }
    }
  })

  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
#- output: Gráficos - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
  
  # gráfico
  
  output$grafico <- renderPlotly({
    dados <- Verificar_Variaveis(input)$Dados
    Colunas_numericas <- Verificar_Variaveis(input)$Colunas_numericas
    colunaQuali <- which(colnames(dados) == input$SelecionarVariaveisQuali)
    colunaQuanti <- which(colnames(dados) == input$SelecionarVariaveisQuant)
    coluna <- which(colnames(dados) == input$SelecionarGLinhas)
    xlab <- input$text_eixo
    main <- input$text_titulo
    X <- dados[,colunaQuali]
    Frequencia <- dados[,colunaQuali]
    
    BASE <- ggplot(data = dados,
                   aes(x = X, fill =  Frequencia ))
    
    Colunas <-  BASE + geom_bar(colour = "black",stat = "count") +
                labs(x = xlab,y = "Frequência") + 
                ggtitle(main) + 
                #scale_fill_discrete (name="Tipos de Espécie") +
                guides(fill=FALSE) + # Remove legend for a particular aesthetic (fill)
                scale_fill_brewer(palette = "Set2") + 
                theme(panel.grid.major = element_line(size = 2),
                      axis.title = element_text(size = 12),
                      axis.text = element_text(size = 10),
                      plot.title = element_text(size = 14),
                      legend.background = element_rect())
    
    Pizza <- ggplot(data = dados,
                    aes(x = "", fill= dados[,colunaQuali]))+
                    geom_bar(width = 1, stat = "count") + coord_polar(theta = "y") + 
                    ggtitle(main) + 
                    theme(axis.title.x = element_blank(),axis.title.y = element_blank())
    
    Histograma <- ggplot(data = dados, aes(x = dados[,colunaQuanti]  )) + 
                  geom_histogram(aes(y = ..density..),
                  colour="black", fill="darkblue", bins = input$nclasses) +
                  ggtitle(main) + labs(x = xlab,y = "Densidade")
    
    
    
    ### Gráficos Univariados
    
      if(input$tipo == "Colunas"){
        if(input$SelecionarVariaveisQuali=="") plot(c(0,10),c(0,10),type="n",
          main="Não Existe Variáveis para esse tipo de gráfico",axes=FALSE,xlab="",ylab="")
        else grafico <- Colunas
      }
      if(input$tipo == "Barras"){
        if(input$SelecionarVariaveisQuali=="") plot(c(0,10),c(0,10),type="n",
          main="Não Existe Variáveis para esse tipo de gráfico",axes=FALSE,xlab="",ylab="")
        else grafico <- Colunas + coord_flip()
      }
      if(input$tipo == "Pizza"){
        if(input$SelecionarVariaveisQuali=="") plot(c(0,10),c(0,10),type="n",
          main="Não Existe Variáveis para esse tipo de gráfico",axes=FALSE,xlab="",ylab="")
        else return(Pizza)
      }
      if(input$tipo == "Histograma"){
        if(input$SelecionarVariaveisQuant=="") plot(c(0,10),c(0,10),type="n",
          main="Não Existe Variáveis para esse tipo de gráfico",axes=FALSE,xlab="",ylab="")
        else grafico <- Histograma
      }

      if(input$tipo == "Linhas") {
        if(input$SelecionarGLinhas == "") plot(c(0,10),c(0,10),type="n",
          main="Não Existe Variáveis para esse tipo de gráfico",axes=FALSE,xlab="",ylab="") else
        if(is.numeric(dados[,coluna])) {
          plot(hist(as.numeric(dados[,coluna]))$mids, hist(as.numeric(dados[,coluna]))$density, type = "b", pch = 16)
        } else {
          plot(as.numeric(table(dados[, coluna])), type = "b", pch = 16)
        }
      }
    ggplotly(grafico)
  })
  
   ### Gráficos Bivariados
  
  output$grafico_Bi <- renderPlotly({
    dados <- Verificar_Variaveis(input)$Dados
    
#    Colunas_numericas <- Verificar_Variaveis(input)$Colunas_numericas
    colunaQuali <- which(colnames(dados) == input$SelecionarVariaveisQuali_Bi)
    colunaQuali2 <- which(colnames(dados) == input$SelecionarVariaveisQuali_Bi2)
    colunaQuanti <- which(colnames(dados) == input$SelecionarVariaveisQuant_Bi)
    colunaQuanti2 <- which(colnames(dados) == input$SelecionarVariaveisQuant_Bi2)
    coluna <- which(colnames(dados) == input$SelecionarGLinhas_Bi)
    coluna2 <- which(colnames(dados) == input$SelecionarGLinhas_Bi2)
    xlab <- input$text_eixo_Bi
    main <- input$text_titulo_Bi
    
    dados2 <- data.frame(X = dados[,colunaQuali],Y = dados[,colunaQuali2])
    dados3 <- data.frame(X = dados[,coluna],Y = dados[,coluna2])
    dados4 <- data.frame(X = dados[,colunaQuanti],Y = dados[,colunaQuanti2])
    
    BASE <- ggplot(data = dados2,
                   aes(x = X, fill =  Y ))
    BASE_Hist <- ggplot(data = dados3,
                   aes(x = X, fill =  Y ))
    BASE_Box <- ggplot(data = dados3,aes(x=Y, y=X, fill=Y))
    BASE_Pontos <- ggplot(dados4, aes(x=X, y=Y) )
    
    Colunas <-  BASE + geom_bar( position = "fill", colour = "lightgreen")
    Colunas2 <- BASE + geom_bar( position = "stack", colour = "lightgreen") 
    Colunas3 <- BASE + geom_bar( position = "dodge", colour = "lightgreen")
    
    Histograma<- BASE_Hist + geom_histogram(bins = input$nclasses_Bi, alpha=.5, 
                                           position="identity")
    BoxPlot <- BASE_Box  + geom_boxplot()
    Pontos <- BASE_Pontos + geom_point(shape=1) + geom_smooth(method=lm)
    
    ### Gráficos Univariados
    
    if(req(input$tipo_Bi) == "Colunas"){
      if(input$SelecionarVariaveisQuali_Bi=="" | input$SelecionarVariaveisQuali_Bi2=="") 
        plot(c(0,10),c(0,10),type="n",main="Não Existe Variáveis para esse tipo de gráfico",
             axes=FALSE,xlab="",ylab="")
      else grafico <- Colunas
    }
    if(input$tipo_Bi == "Colunas2"){
      if(input$SelecionarVariaveisQuali_Bi=="" | input$SelecionarVariaveisQuali_Bi2=="") 
        plot(c(0,10),c(0,10),type="n",main="Não Existe Variáveis para esse tipo de gráfico",
             axes=FALSE,xlab="",ylab="")
      else grafico <- Colunas2
    }
    if(input$tipo_Bi == "Colunas3"){
      if(input$SelecionarVariaveisQuali_Bi=="" | input$SelecionarVariaveisQuali_Bi2=="") 
        plot(c(0,10),c(0,10),type="n",main="Não Existe Variáveis para esse tipo de gráfico",
             axes=FALSE,xlab="",ylab="")
      else grafico <- Colunas3
    }
    if(input$tipo_Bi == "Barras"){
      if(input$SelecionarVariaveisQuali_Bi=="" | input$SelecionarVariaveisQuali_Bi2=="") 
        plot(c(0,10),c(0,10),type="n",main="Não Existe Variáveis para esse tipo de gráfico",
             axes=FALSE,xlab="",ylab="")
      else grafico <- Colunas + coord_flip()
    }
    if(input$tipo_Bi == "Barras2"){
      if(input$SelecionarVariaveisQuali_Bi=="" | input$SelecionarVariaveisQuali_Bi2=="") 
        plot(c(0,10),c(0,10),type="n",main="Não Existe Variáveis para esse tipo de gráfico",
             axes=FALSE,xlab="",ylab="")
      else grafico <- Colunas2 + coord_flip()
    }
    if(input$tipo_Bi == "Barras3"){
      if(input$SelecionarVariaveisQuali_Bi=="" | input$SelecionarVariaveisQuali_Bi2=="") 
        plot(c(0,10),c(0,10),type="n",main="Não Existe Variáveis para esse tipo de gráfico",
             axes=FALSE,xlab="",ylab="")
      else grafico <- Colunas3 + coord_flip()
    }
    
    if(input$tipo_Bi == "Histograma"){
      if(input$SelecionarGLinhas_Bi=="" | input$SelecionarGLinhas_Bi2=="")
        plot(c(0,10),c(0,10),type="n",main="Não Existe Variáveis para esse tipo de gráfico",
             axes=FALSE,xlab="",ylab="")
      else grafico <- Histograma
    }
    
    if(input$tipo_Bi == "BoxPlot"){
      if(input$SelecionarGLinhas_Bi=="" | input$SelecionarGLinhas_Bi2=="")
        plot(c(0,10),c(0,10),type="n",main="Não Existe Variáveis para esse tipo de gráfico",
             axes=FALSE,xlab="",ylab="")
      else grafico <- BoxPlot
    }
    if(input$tipo_Bi == "Pontos"){
      if(input$SelecionarVariaveisQuant_Bi=="" | input$SelecionarVariaveisQuant_Bi2=="") 
        plot(c(0,10),c(0,10),type="n",main="Não Existe Variáveis para esse tipo de gráfico",
             axes=FALSE,xlab="",ylab="")
      else grafico <- Pontos
    }

    ggplotly(grafico)
  })
  
  

  
}     ######################################### Final do server
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# Final Server



shinyApp(ui, server)


