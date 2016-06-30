
#----------------------------# UI elementos #----------------------------------#

# Escolher Exemplo ou entrada de dados
Ex_ou_enter <-
radioButtons(inputId = "Escolha", label = "Qual banco de dados quer usar?",
             c("Exemplo", "Seu própio banco de dados"))

# Escolher o exemplo
enter_exemplo <-
selectInput(inputId = "ExemplodeDados", label = "Exemplos de bancos de dados",
            choices = list("faithful" = "faithful", "cars" = "cars" ,
                           "iris" = "iris", "WorldPhones" = "WorldPhones", "mtcars" = "mtcars"), 
            selected = "mtcars")

# Entrar com o banco de dados
enter_dados <-
  fileInput(inputId = "arquivoescolhido", label = "Escolha o seu arquivo .csv",
            accept = c(
              "text/csv",
              "text/comma-separated-values",
              "text/tab-separated-values",
              "text/plain",
              ".csv"
            ))

# Selecionar se 1 linha é título da coluna
Header <-
  checkboxInput(inputId = "header", label = "Header", TRUE)

# Escolher tipo de separador
Separador <-
  radioButtons(inputId = "sep", label = "Separador",
               c( Virgula = ",", Ponto_e_Virgula = ";", Tab = "\t"),
               ";")

# Escolher caractere para decimal
Decimal <-
radioButtons(inputId = "dec", label = "Decimal",
             c( Virgula = ",", ponto = "."),
             ".")

# Selecionar o tipo de gráfico
TipoGrafico <-
  selectInput(inputId = "tipo", label = "Tipo de gráfico",
              choices = c("Colunas", "Barras", "Pizza", "Histograma", "Linhas"))

# Selecionar o tipo de gráfico bivariado
TipoGrafico_Bi <-
  selectInput(inputId = "tipo_Bi", label = "Tipo de gráfico",
              choices = c("Colunas","Colunas2","Colunas3", "Barras","Barras2","Barras3",
                          "Histograma", "Linhas", "Pontos", "BoxPlot"))


# inserir o título do gráfico
InserirTitulo <-
  textInput("text_titulo", label = "Título", value = "Digite o título do gráfico")

InserirTitulo_Bi <-
  textInput("text_titulo_Bi", label = "Título", value = "Digite o título do gráfico")

# inserir o eixo x do gráfico
InserirEixo <-
  textInput("text_eixo", label = "Eixo x", value = "Digite o título do eixo x")

# inserir número de classes do histograma
NumeroClasses <-
sliderInput("nclasses", label = "Número de Classes", min = 5, 
            max = 50, value = 10)

NumeroClasses_Bi <-
  sliderInput("nclasses_Bi", label = "Número de Classes", min = 5, 
              max = 50, value = 10)






#----------------------------# UI Funções #------------------------------------#

# selecionar as variáveis qualitativas
UIselectqualitativa <- function(nomes)
  selectizeInput(inputId = "SelecionarQualitativa", "Selecione as variáveis qualitativas!",
                choices = nomes , multiple = TRUE)

# selecionar as variáveis para visualizar
UIvervariaveis <- function(nomes)
  checkboxGroupInput('show_vars', 
                     'Colunas a mostrar:', 
                     nomes,
                     selected = nomes[1])

# Selecionar variáveis para gráfico de linhas
UIvarlinhas <- function(nomes)
  selectizeInput(inputId = "SelecionarGLinhas", "Selecione a variável!",
                 choices = nomes , multiple = FALSE)

UIvarlinhas_Bi <- function(nomes)
  selectizeInput(inputId = "SelecionarGLinhas_Bi", "Selecione a variável quantitativa (X)",
                 choices = nomes , multiple = FALSE)
UIvarlinhas_Bi2 <- function(nomes)
  selectizeInput(inputId = "SelecionarGLinhas_Bi2", "Selecione a variável qualitativa (Y)",
                 choices = nomes , multiple = FALSE)

# Selecionar variáveis qualitativas para o gráfico
UIvarquali <- function(nomes)
  selectizeInput(inputId = "SelecionarVariaveisQuali", "Selecione a variável!",
                 choices = nomes, multiple = FALSE)

UIvarquali_Bi <- function(nomes)
  selectizeInput(inputId = "SelecionarVariaveisQuali_Bi", "Selecione a variável (X)!",
                 choices = nomes, multiple = FALSE)
UIvarquali_Bi2 <- function(nomes)
  selectizeInput(inputId = "SelecionarVariaveisQuali_Bi2", "Selecione a variável (Y)!",
                 choices = nomes, multiple = FALSE)

# Selecionar variáveis quantitativas para o gráfico
UIvarquanti <- function(nomes)
selectizeInput(inputId = "SelecionarVariaveisQuant", "Selecione a variável!",
               choices = nomes, multiple = FALSE)

UIvarquanti_Bi <- function(nomes)
  selectizeInput(inputId = "SelecionarVariaveisQuant_Bi", "Selecione a variável (X)!",
                 choices = nomes, multiple = FALSE)
UIvarquanti_Bi2 <- function(nomes)
  selectizeInput(inputId = "SelecionarVariaveisQuant_Bi2", "Selecione a variável (Y)!",
                 choices = nomes, multiple = FALSE)







