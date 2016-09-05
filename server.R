# Início Server
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#

shinyServer(
  function(input, output, session)
  {
    
    #- output: renderUI - nomes das colunas- - - - - - - - - - - - - - - - - - - - #
    
    # nome das colunas para selecionar as qualitativas
    output$nomes_das_colunas <- renderUI({
      dados <- dados(input)
      nomes <- as.matrix(colnames(dados$dados))
      
      UIselectqualitativa(nomes)
    })
    
    # função para a seleção das colunas a visualizar do banco de dados
    output$nomes_das_colunas2 <- renderUI({
      dados <- dados(input)
      nomes <- as.matrix(colnames(dados$dados))
      
      UIvervariaveis(nomes)
    })
    
    # Nome das colunas para fazer o gráfico de linhas
    output$nomes_das_colunas3 <- renderUI({
      dados <- dados(input)
      nomes <- as.matrix(colnames(dados$dados))
      
      UIvarlinhas(nomes)
    })
    # Nome das colunas para fazer o gráfico de linhas bivariado
    output$nomes_das_colunas3_Bi <- renderUI({
      dados <- dados(input)
      colunas_numericas <- Verificar_Variaveis(input)$Colunas_numericas
      
      if(length(colunas_numericas) == ncol(dados$dados))
        nomes <- as.matrix(colnames(dados$dados))
      else{
        if(colunas_numericas[1] == 0)
          nomes <- as.matrix("")
        else
          nomes <- as.matrix(colnames(dados$dados[colunas_numericas]))
      }
      
      UIvarlinhas_Bi(nomes)
    })
    output$nomes_das_colunas3_Bi2 <- renderUI({
      dados <- dados(input)
      colunas_numericas <- Verificar_Variaveis(input)$Colunas_numericas
      
      if(colunas_numericas[1] == 0)
        nomes <- as.matrix(colnames(dados$dados))
      else{
        if(length(colunas_numericas) == ncol(dados$dados))
          nomes <- as.matrix("")
        else
          nomes <- as.matrix(colnames(dados$dados[-colunas_numericas]))
      }
      
      UIvarlinhas_Bi2(nomes)
    })
    # função para retornar o nome das colunas qualitativas para o gráfico
    output$nomes_das_colunas_quali <- renderUI({
      dados <- dados(input)
      colunas_numericas <- Verificar_Variaveis(input)$Colunas_numericas
      
      if(colunas_numericas[1] == 0)
        nomes <- as.matrix(colnames(dados$dados))
      else{
        if(length(colunas_numericas) == ncol(dados$dados))
          nomes <- as.matrix("")
        else
          nomes <- as.matrix(colnames(dados$dados[-colunas_numericas]))
      }
      UIvarquali(nomes)
    })
    
    # função para retornar o nome das colunas qualitativas para o gráfico bivariado
    output$nomes_das_colunas_quali_Bi <- renderUI({
      dados <- dados(input)
      colunas_numericas <- Verificar_Variaveis(input)$Colunas_numericas
      
      if(colunas_numericas[1] == 0)
        nomes <- as.matrix(colnames(dados$dados))
      else{
        if(length(colunas_numericas) == ncol(dados$dados))
          nomes <- as.matrix("")
        else
          nomes <- as.matrix(colnames(dados$dados[-colunas_numericas]))
      }
      UIvarquali_Bi(nomes)
    })
    
    output$nomes_das_colunas_quali_Bi2 <- renderUI({
      dados <- dados(input)
      colunas_numericas <- Verificar_Variaveis(input)$Colunas_numericas
      
      if(colunas_numericas[1] == 0)
        nomes <- as.matrix(colnames(dados$dados))
      else{
        if(length(colunas_numericas) == ncol(dados$dados))
          nomes <- as.matrix("")
        else
          nomes <- as.matrix(colnames(dados$dados[-colunas_numericas]))
      }
      UIvarquali_Bi2(nomes)
    })
    
    # função para retornar o nome das colunas quantitativas para o gráfico
    output$nomes_das_colunas_quanti <- renderUI({
      dados <- dados(input)
      colunas_numericas <- Verificar_Variaveis(input)$Colunas_numericas
      
      if(length(colunas_numericas) == ncol(dados$dados))
        nomes <- as.matrix(colnames(dados$dados))
      else{
        if(colunas_numericas[1] == 0)
          nomes <- as.matrix("")
        else
          nomes <- as.matrix(colnames(dados$dados[colunas_numericas]))
      }
      UIvarquanti(nomes)
    })
    
    # função para retornar o nome das colunas quantitativas para o gráfico bivariado
    output$nomes_das_colunas_quanti_Bi <- renderUI({
      dados <- dados(input)
      colunas_numericas <- Verificar_Variaveis(input)$Colunas_numericas
      
      if(length(colunas_numericas) == ncol(dados$dados))
        nomes <- as.matrix(colnames(dados$dados))
      else{
        if(colunas_numericas[1] == 0)
          nomes <- as.matrix("")
        else
          nomes <- as.matrix(colnames(dados$dados[colunas_numericas]))
      }
      UIvarquanti_Bi(nomes)
    })
    
    output$nomes_das_colunas_quanti_Bi2 <- renderUI({
      dados <- dados(input)
      colunas_numericas <- Verificar_Variaveis(input)$Colunas_numericas
      
      if(length(colunas_numericas) == ncol(dados$dados))
        nomes <- as.matrix(colnames(dados$dados))
      else{
        if(colunas_numericas[1] == 0)
          nomes <- as.matrix("")
        else
          nomes <- as.matrix(colnames(dados$dados[colunas_numericas]))
      }
      UIvarquanti_Bi2(nomes)
    })
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    #- output: tabelas dos dados, frequencia - - - - - - - - - - - - - - - - - - - #
    
    # Tabela com os dados
    output$Dados <- renderDataTable({
      dados <- dados(input)
      #    as.matrix(dados$dados[,input$show_vars, drop = FALSE])
      dados$dados[req(input$show_vars)]
    }, options = list(pageLength = 10)
    )
    
    # table da tabela com as variáveis quantitativas ( númericas )
    mytable1 <- reactive({
      dados <- dados(input)
      
      Colunas_numeric <- Verificar_Variaveis(input)$Colunas_numericas
      dados <- Verificar_Variaveis(input)$Dados
      if(length(Colunas_numeric) == 1){
        if(Colunas_numeric == 0) as.matrix("") else
          Sumario(dados[Colunas_numeric])
      }else
        Sumario(dados[Colunas_numeric])
    })
    
    output$TabelaDescritiva_quantitativa <- renderTable({
      mytable1()
    })
    
    # table da tabela com as variáveis qualitativas ( fatores )
    mytable2 <- reactive({
      dados <- dados(input)
      
      Colunas_numeric <- Verificar_Variaveis(input)$Colunas_numericas
      dados <- Verificar_Variaveis(input)$Dados
      if(length(Colunas_numeric) == ncol(dados) ) as.matrix("") 
      else{
        if(length(Colunas_numeric) == 1){
          if(Colunas_numeric == 0) summary(dados)else
            summary(dados[-Colunas_numeric])
        }else
          summary(dados[-Colunas_numeric])
      }
      
    })
    
    output$TabelaDescritiva_qualitativa <- renderTable({
      mytable2()
    })
    
    
    # função para download da Tabela em .csv, pdf etc
    output$downloadData <- downloadHandler(
      
      filename = function(){
        if(req(input$format_Tabela) == ".CSV")
          "Tabela com estatisticas.csv"
        else
          paste('Tabela com estatisticas', sep = '.', switch(
            input$format_Tabela, PDF = 'pdf', HTML = 'html', Word = 'docx'
          ))
      },
      content = function(file) {
        if(req(input$format_Tabela) == ".CSV"){
          tabela <- mytable1()
          tabela <- data.frame( Estatísticas = rownames(tabela), round( tabela, 2) )
          write.table( tabela, file, row.names = FALSE, sep = ";", dec = ",") 
        }
        else{
          aux <- "quantitativa"
          src <- normalizePath('04-export_document.Rmd')
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          file.copy(src, '04-export_document47.Rmd')
          
          out <- render('04-export_document47.Rmd', switch(
            input$format_Tabela,
            PDF = pdf_document(), HTML = html_document(), Word = word_document()
          ))
          file.rename(out, file)
        }
        
      }
    )
    
    # função para download da Tabela em .csv, pdf etc
    output$downloadData2 <- downloadHandler(
      
      filename = function(){
        if(req(input$format_Tabela) == ".CSV")
          "Tabela de frequencias.csv"
        else
          paste('Tabela de frequencias', sep = '.', switch(
            input$format_Tabela, PDF = 'pdf', HTML = 'html', Word = 'docx'
          ))
      },
      content = function(file) {
        if(req(input$format_Tabela) == ".CSV")
          write.table(mytable2(), file, row.names = FALSE, sep = ";", dec = ",")
        else{
          aux <- "qualitativa"
          src <- normalizePath('04-export_document.Rmd')
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          file.copy(src, '04-export_document7.Rmd')
          
          out <- rmarkdown::render('04-export_document7.Rmd', switch(
            input$format_Tabela,
            PDF = pdf_document(), HTML = html_document(), Word = word_document()
          ))
          file.rename(out, file)
        }
        
      }
    )
    
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    #- output: Gráficos - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    
    # gráfico
    
    output$grafico <- renderPlotly({
      dados <- Verificar_Variaveis(input)$Dados
      #    Colunas_numericas <- Verificar_Variaveis(input)$Colunas_numericas
      
      # req( all(Colunas_numericas) != 0)
      # req(length(Colunas_numericas) < ncol(dados) )
      
      #    coluna <- which(colnames(dados) == input$SelecionarGLinhas)
      
      xlab <- input$text_eixo
      main <- input$text_titulo
      Tema <- theme(panel.grid.major = element_line(size = 2),
                    axis.title = element_text(size = 12),
                    axis.text = element_text(size = 10),
                    plot.title = element_text(size = 14),
                    legend.background = element_rect())
      
      
      ### Gráficos Univariados
      
      if(any(c("Barras","Setores","Colunas") == input$tipo)){
        req(input$SelecionarVariaveisQuali)
        
        ## número da coluna qualitativa
        colunaQuali <- which(colnames(dados) == input$SelecionarVariaveisQuali) 
        
        X <- dados[,colunaQuali]
        #        Frequencia <- dados[,colunaQuali]
        
        BASE <- ggplot(data = dados, aes(x = X, fill =  X ))
        
        Colunas <-  BASE + geom_bar(colour = "black",stat = "count") +
          labs(x = xlab,y = "Frequência") + 
          ggtitle(main) + Tema +
          guides(fill=FALSE) # Remove legend for a particular aesthetic (fill)
        
        Setores <- plot_ly(data.frame(X = names( table(X)), x = as.numeric( table(X)) ) , 
                           labels = X, values = x, type = "pie") %>% layout(title = main)
        
        
        if(input$tipo == "Barras") grafico <- Colunas + coord_flip() 
        if(input$tipo == "Setores") grafico <- Setores
        if(input$tipo == "Colunas") grafico <- Colunas
      }
      
      if(any(c("Histograma") == input$tipo)){
        req(input$SelecionarVariaveisQuant)
        
        colunaQuanti <- which(colnames(dados) == input$SelecionarVariaveisQuant)
        X <- dados[,colunaQuanti]
        Histograma <- ggplot(data = dados, aes(x = X)) + 
          geom_histogram(aes(y = ..density..),
                         colour="black", fill="darkblue", bins = input$nclasses) +
          ggtitle(main) + labs(x = xlab,y = "Densidade") + Tema
        
        grafico <- Histograma
      }
      
      if(input$tipo == "Setores") grafico else ggplotly(grafico)
    })
    
    ### Gráficos Bivariados
    
    output$grafico_Bi <- renderPlotly({
      
      dados <- Verificar_Variaveis(input)$Dados
      
      main <- input$text_titulo_Bi
      Tema <- theme(panel.grid.major = element_line(size = 2),
                    axis.title = element_text(size = 12),
                    axis.text = element_text(size = 10),
                    plot.title = element_text(size = 14),
                    legend.background = element_rect())
      
      ### Gráficos Bivariados
      #------- Só variáveis qualitativas
      if( any(c("Colunas Superpostas Prop.","Colunas Superpostas","Colunas Justapostas", 
                "Linhas qualitativas","Barras Superpostas Prop.",
                "Barras Superpostas","Barras Justapostas") == input$tipo_Bi) ){
        req(input$SelecionarVariaveisQuali_Bi, input$SelecionarVariaveisQuali_Bi2)
        
        colunaQuali <- which(colnames(dados) == input$SelecionarVariaveisQuali_Bi)
        colunaQuali2 <- which(colnames(dados) == input$SelecionarVariaveisQuali_Bi2)
        
        dados2 <- data.frame(X = dados[,colunaQuali],Y = dados[,colunaQuali2])
        
        BASE <- ggplot(data = dados2, aes(x = X, fill =  Y ))
        
        Colunas <-  BASE + geom_bar( position = "fill", colour = "black") +
          labs(y = "Frequência Relativa") + Tema + ggtitle(main)
        Colunas2 <- BASE + geom_bar( position = "stack", colour = "black") +
          labs(y = "Frequência") + Tema + ggtitle(main)
        Colunas3 <- BASE + geom_bar( position = "dodge", colour = "black") +
          labs(y = "Frequência") + Tema + ggtitle(main)
        
        aux <- count(dados2, vars = c("X","Y"))
        Linhas <- ggplot(data = aux, aes( x = X, y = freq ,group = Y, colour = Y)) + geom_line() +
          geom_point() + Tema + ggtitle(main) + labs(x = "X", y = "Frequência")
        
        if(input$tipo_Bi == "Colunas Superpostas Prop.") grafico <- Colunas
        if(input$tipo_Bi == "Colunas Superpostas") grafico <- Colunas2
        if(input$tipo_Bi == "Colunas Justapostas") grafico <- Colunas3
        if(input$tipo_Bi == "Barras Superpostas Prop.") grafico <- Colunas + coord_flip()
        if(input$tipo_Bi == "Barras Superpostas") grafico <- Colunas2 + coord_flip()
        if(input$tipo_Bi == "Barras Justapostas") grafico <- Colunas3 + coord_flip()
        if(input$tipo_Bi == "Linhas qualitativas") grafico <- Linhas
      }
      #------- Variáveis qualitativa e quantitativa
      if( any(c("Histograma", "BoxPlot", "Linhas densidades") == input$tipo_Bi) ){
        req(input$SelecionarGLinhas_Bi, input$SelecionarGLinhas_Bi2)
        
        coluna <- which(colnames(dados) == input$SelecionarGLinhas_Bi)
        coluna2 <- which(colnames(dados) == input$SelecionarGLinhas_Bi2)
        
        dados3 <- data.frame(X = dados[,coluna],Y = dados[,coluna2])
        
        BASE_Hist <- ggplot( data = dados3, aes(x = X, fill =  Y ) )
        BASE_Box <- ggplot( data = dados3, aes(x=Y, y=X, fill=Y) )
        
        Histograma<- BASE_Hist + 
          geom_histogram( bins=input$nclasses_Bi, alpha=.7, position="identity") + 
          Tema + labs(y = "Frequência") + ggtitle(main)
        
        BoxPlot <- BASE_Box + geom_boxplot() + Tema + labs(x = "", y = "X") + ggtitle(main)
        
        if(input$tipo_Bi == "Histograma") grafico <- Histograma
        if(input$tipo_Bi == "BoxPlot") grafico <- BoxPlot
        if(input$tipo_Bi == "Linhas densidades")
          grafico <- ggplot(dados3, aes(X, fill = Y)) + geom_density(alpha = 0.2) + 
          Tema + ggtitle(main) + labs( y = "Densidade")
      }
      
      #------- Só variáveis quantitativas
      if(any(c("Pontos") == input$tipo_Bi) ){
        req(input$SelecionarVariaveisQuant_Bi, input$SelecionarVariaveisQuant_Bi2)
        
        colunaQuanti <- which(colnames(dados) == input$SelecionarVariaveisQuant_Bi)
        colunaQuanti2 <- which(colnames(dados) == input$SelecionarVariaveisQuant_Bi2)
        
        dados4 <- data.frame(X = dados[,colunaQuanti],Y = dados[,colunaQuanti2])
        
        BASE_Pontos <- ggplot(dados4, aes(x=X, y=Y) )
        Pontos <- BASE_Pontos + geom_point(shape=1) + geom_smooth(method=lm) + Tema + ggtitle(main)
        
        grafico <- Pontos
      }
      
      ggplotly(grafico)
    })
    
    
    
    
  }     ######################################### Final do server
  
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
# Final Server

