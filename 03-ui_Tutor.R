Tutorial <-
box( title = "", status = "danger", width = 15,
       p("  Este aplicativo tem por finalidade ajudar 
       os usuários a visualizar, de forma iterativa,
       estatísticas descritivas básicas de um 
       determinado banco de dados."),
     p("Abaixo segue um tutorial para uso do aplicativo de forma correta,
       o qual facilitará a compreensão dos resultados, tabelas e gráficos."),
     h3("1. Guia", span("Dados", style = "color:blue")),
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
     img(src = "imagem2.png", height = 200 ,width = 300),
     br(),br(),
     h4("1.3 Seleção do seu banco de dados .csv"),
     p("Através do botão", strong("Selecionar arquivo..."), "você poderá enviar 
       o seu arquivo com o banco de dados em formato .csv, onde as colunas do 
       banco de dados devem ser as variáveis estudadas."),
     img(src = "imagem3.JPG", width = 300),
     h5("1.3.1 Configurações para ler os dados"),
     p("Você poderá customizar algumas opções para abrir o banco de dados de 
       forma correta."),
     img(src = "imagem4.JPG"),
     p("Marque a caixa", strong("Header"),"caso você queira que a primeira linha
       do banco de dados seja considerado o nome das variáveis."),
     p("Na seção de marcadores", strong("Separador"),"você poderá escolher o
       separador de células."),
     p("Na seção de marcadores", strong("Decimal"),"você poderá escolher a
       string de décimos dos números."),
     br(),
     h4("1.4 Visualização das variáveis do banco de dados"),
     p("Através da seção de marcadores", strong("Colunas a mostrar:"),
       "você poderá selecionar as variáveis que queira visualizar."),
     img(src = "imagem5.JPG"),
     br(),br(),
     h4("1.5 Visualização do banco de dados"),
     p("Agora você poderá visualizar o banco de dados, e usar as caixas de texto
        para fazer buscas específicas, e as abas para navegar no banco de dados,
       veja a imagem a seguir, com a visualização das variáveis do exemplo mtcars"),
     img(src = "imagem6.JPG"),
     br(),br(),
     h3("2. Guia" , span("Descritiva", style = "color:blue")),
     p("Aqui você poderá determinar as variáveis qualitativas e quantitativas, 
       e visualizar estatísticas descritivas para as respectivas variáveis."),
     h4("2.1 Seleção das variáveis qualitativas"),
     p("Na caixa de seleção múltipla", strong("Selecione as variáveis qualitativas!"), 
     "você poderá selecionar as variáveis que são qualitativas, porém que são dadas 
      de forma numérica, por exemplo: Sexo Masculino - 1, Sexo Feminino - 2. Se a 
      variável for qualitativa e for uma string, não é necessário selecioná-las 
      pois o aplicativo já as reconhece como um fator."),
     img(src = "imagem7.jpg"),
     br(),br(),
     h4("2.2 Visualização das tabelas descritivas"),
     p("Através das abas", strong("Variáveis quantitativas"), "e", 
       strong("Variáveis qualitativas"), "você poderá visualizar as estatísticas 
       descritivas das variáveis quantitativas e a tabela de frequências das
       variáveis qualitativas."),
     img(src = "imagem8.jpg",width = 800),
     h3("3. Guia ", span("Gráficos Univariados", style = "color:blue")),
     p("Nessa guia você poderá criar e salvar seus gráficos univariados."),
     h4("3.1 Escolha do gráfico"),
     p("Na caixa de seleção", strong("Tipo de gráfico"), "você poderá escolher
      o tipo de gráfico desejado: Colunas, Barras, Pizza e Histograma e na caixa 
      de seleção" , strong("Selecione a variável!"), "aparecerá as variáveis 
      adequadas para cada gráfico."),
     img(src = "imagem9.jpg"),
     h4("3.2 Formatação do gráfico"),
     p("Nas caixas de texto", strong("Título"), "e", strong("Eixo x"), "você poderá
       colocar o título do gráfico e do Eixo x."),
     img(src = "imagem10.jpg"),
     h4("3.3 Visualização do gráfico univariado"),
     p("Depois de escolhido o tipo de gráfico e a variável, você poderá visualizar 
       o gráfico univariado, como por exemplo o gráfico de pizza da variável",
       em("carb"), "do banco de dados", em("mtcars"), "que é dado na imagem abaixo."),
     img(src = "imagem11.jpg"),
     h3("4. Guia ", span("Gráficos Bivariados", style = "color:blue")),
     p("Nessa guia você poderá criar e salvar seus gráficos Bivariados."),
     h4("4.1 Escolha do gráfico"),
     p("Na caixa de seleção", strong("Tipo de gráfico"), "você poderá escolher
       o tipo de gráfico desejado: Colunas Empilhadas, Colunas Agrupadas,
       Barras Empilhadas, Barras Agrupadas, Histograma, Linhas, Box-Plot, Pontos,
       e nas caixas de seleção de variáveis você deve selecionar as duas variáveis 
       adequadas para cada gráfico."),
     img(src = "imagem12.jpg"),
     h4("4.2 Formatação do gráfico"),
     p("Na caixa de texto", strong("Título"), "você poderá
       colocar o título do gráfico bivariado."),
     img(src = "imagem13.jpg"),
     h4("4.3 Visualização do gráfico Bivariado"),
     p("Depois de escolhido o tipo de gráfico e as variáveis a serem analisadas,
       você poderá visualizar o gráfico Bivariado, como por exemplo o gráfico Box-Plot
       das variáveis", em("mpg"), "e", em("gear"), "do banco de dados", em("mtcars"), 
       "que é dado na imagem abaixo."),
     img(src = "imagem14.jpg")
     
     
)