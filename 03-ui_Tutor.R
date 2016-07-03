Tutorial <-
box( title = "", status = "danger", width = 15,
     p("  Este aplicativo tem por finalidade ajudar 
       os usuários a visuaizar, de forma iterativa,
       estatísticas descritivas básicas de um 
       determinado banco de dados."),
     p("Abaixo segue um tutorial para uso do aplicativo de forma correta,
       o qual facilitará a compreensão dos resultados, tabelas e gráficos,
       além das entradas de informações por você."),
     h3("1. Vá na guia", span("Dados", style = "color:blue")),
     p("Aqui você poderá escolher se irá trabalhar com um banco de dados
       já disponível nos exemplos, ou com o seu próprio banco de dados."),
     h4("1.1 Escolha os dados"),
     p("Através dos marcadores",strong("Exemplo")," e ",strong("Seu próprio banco de dados"),
       "você poderá escolher se deseja usar um exemplo ou enviar um banco de dados
       em arquivo .csv"),
     img(src = "imagem1.JPG", height = 150, width = 300),
     br(),br(),
     h4("1.2 Seleção do Exemplo"),
     p("Através da caixa", strong("Exemplos de bancos de dados"), 
       "você poderá selecionar alguns dos bancos de dados que estão 
       disponíveis no próprio software R."),
     img(src = "imagem2.PNG", width = 300),
     br(),br(),
     h4("1.3 Seleção do seu banco de dados .csv"),
     p("Através do botão", strong("Selecionar arquivo..."), "você poderá enviar 
       o seu arquivo com o banco de dados em formato .csv, onde as colunas do 
       banco de dados devem ser as variáveis estudadas."),
     img(src = "imagem3.jpg", width = 300),
     h5("1.3.1 Configurações para ler os dados"),
     p("Você poderá customizar algumas opções para abrir o banco de dados de 
       forma correta."),
     img(src = "imagem4.jpg"),
     p("Marque a caixa", strong("Header"),"caso você queira que a primeira linha
       do banco de dados seja considerado o nome das variáveis."),
     p("Na seção de marcadores", strong("Separador"),"você poderá escolher o
       separador de células."),
     p("Na seção de marcadores", strong("Decimal"),"você poderá escolher a
       string de décimos dos números."),
     br(),
     h4("1.4 Visualização das variáveis do banco de dados"),
     p("Através da seção de marcadores", strong("Colunas a mostrar:"),
       "você poderá selecionar as variáveis que queira visulizar."),
     img(src = "imagem5.jpg"),
     br(),br(),
     h4("1.5 Visualização do banco de dados"),
     p("Agora você poderá visualizar o banco de dados, e usar as caixas de texto
        para fazer buscas específicas, e as abas para navegar no banco de dados,
       veja a imagem a seguir, com a visualização das variáveis do exemplo mtcars"),
     img(src = "imagem6.jpg"),
     br(),br()
     
     
     
)