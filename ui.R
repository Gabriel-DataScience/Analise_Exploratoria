#----------------------------- Atividade Final -------------------------------#
#------------------------ Estatística computacional --------------------------#
#--------------------------- Lissandra e Gabriel -----------------------------#
source("00-init.R", encoding = "UTF-8")
source("02-ui_elements.R", encoding = "UTF-8")
source("01-fnct-plot.R", encoding = "UTF-8")
source("03-ui_Tutor.R", encoding = "UTF-8")
# Início Pagina do Usuário
#------------------------------------------------------------------------------#

shinyUI(
  dashboardPage( skin = "red",
                 dashboardHeader(title = "Análise Exploratória"),
                 dashboardSidebar(
                   sidebarMenu(
                     menuItem("Tutorial", tabName = "Tutorial", icon = icon("fa fa-book")),
                     menuItem("Dados", tabName = "Dados", icon = icon("fa fa-usb")),
                     menuItem("Descritiva", tabName = "Descritiva", icon = icon("fa fa-list-alt")),
                     menuItem("Gráficos Univariados", tabName = "Graficos", icon = icon("fa fa-pie-chart")),
                     menuItem("Gráficos Bivariados", tabName = "Graficos_Bi", icon = icon("fa fa-line-chart"))
                   ),
                   sidebarUserPanel(strong("Autores: Gabriel Fernandes"),
                                    subtitle = strong("e Lissandra parente")
                   ),
                   sidebarUserPanel("Universidade Federal do ceará,",
                                    subtitle = a(href = "http://www.dema.ufc.br/",
                                                 "Estatística, 2016")
                   )
                   
                 ),
                 dashboardBody(
                   tabItems(
                     tabItem(tabName = "Tutorial",
                             titlePanel("Tutorial para usar o aplicativo"),
                             box(title = "", status = "danger", width = 15,
                                 solidHeader = FALSE,
                                 p(strong("Produção:"),"Gabriel Fernandes e Lissandra Parente"),
                                 p("Estatística, Universidade Federal do Ceará, 2016.")),
                             Tutorial
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
                               fluidRow(
                                 box( title = "Variáveis qualitativas", solidHeader = TRUE, status = "danger",
                                      uiOutput("nomes_das_colunas")
                                 ),
                                 box( title = "Download das tabelas", solidHeader = FALSE, status = "danger", 
                                      Download_quantitativa,
                                      Download_qualitativa,
                                      format_arquivo_Tabela
                                 )
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
                                            input.tipo == 'Setores' ",
                                            uiOutput("nomes_das_colunas_quali")
                                          ), 
                                          conditionalPanel(
                                            condition = "input.tipo == 'Histograma'",
                                            uiOutput("nomes_das_colunas_quanti")
                                          )
                                      ),
                                      box(title = "Formatação", solidHeader = TRUE, status = "danger",
                                          width = NULL,
                                          
                                          InserirTitulo,
                                          conditionalPanel(
                                            condition = "input.tipo == 'Colunas' | 
                                            input.tipo == 'Barras' |
                                            input.tipo == 'Histograma'",
                                            InserirEixo
                                          ),
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
                                            "input.tipo_Bi == 'Colunas Superpostas Prop.' |
                               input.tipo_Bi == 'Colunas Superpostas' |
                               input.tipo_Bi == 'Colunas Justapostas' |
                               input.tipo_Bi == 'Barras Superpostas Prop.'|
                               input.tipo_Bi == 'Barras Superpostas'|
                               input.tipo_Bi == 'Barras Justapostas'|
                               input.tipo_Bi == 'Linhas qualitativas'",
                                          uiOutput("nomes_das_colunas_quali_Bi"),
                                          uiOutput("nomes_das_colunas_quali_Bi2")
                                        ), 
                                        conditionalPanel(
                                          condition = "input.tipo_Bi == 'Pontos'",
                                          uiOutput("nomes_das_colunas_quanti_Bi"),
                                          uiOutput("nomes_das_colunas_quanti_Bi2")
                                        ),
                                        conditionalPanel(
                                          condition = "input.tipo_Bi == 'Linhas densidades'|
                               input.tipo_Bi == 'Histograma' |
                               input.tipo_Bi == 'BoxPlot'",
                                          uiOutput("nomes_das_colunas3_Bi"),
                                          uiOutput("nomes_das_colunas3_Bi2"))
                                    ),
                                    box(title = "Formatação", solidHeader = TRUE, status = "danger",
                                        width = NULL,
                                        InserirTitulo_Bi,
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
  
)
#------------------------------------------------------------------------------#
# Final do layout


