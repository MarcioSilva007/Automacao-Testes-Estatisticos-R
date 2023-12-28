# Estatística na Prática - Dashboard com Shiny Para Automação de Testes Estatísticos


# Instale cada um dos pacotes abaixo executando os comandos no console do RStudio.

#install.packages("shiny")
#install.packages("shinyjs")
#install.packages("shinyvalidate")
#install.packages("shinycssloaders")
#install.packages("tidyverse")
#install.packages("broom")
#install.packages("bslib")
#install.packages("thematic")
#install.packages("DT")
#install.packages("plotly")

# Imports
library(shiny)
library(shinyjs)
library(shinyvalidate)
library(shinycssloaders)
library(tidyverse)
library(broom)
library(bslib)
library(thematic)
library(DT)
library(plotly)
options(warn = -1)

# Carrega o arquivo com a descrição dos testes
dados_desc_te <- read.csv("desc_testes_estatisticos.csv")

# UI - User Interface

# Cria a página de navegação 
ui <- navbarPage(
  
  # Cria instância do shinyjs
  shinyjs::useShinyjs(),

  # Tema de cores do dashboard
  theme = bs_theme(version = 5,
                   bootswatch = "flatly",
                   primary = "#3f7928",
                   secondary = "#5340f7",
                   success = "#dadeba"
  ),
  
  # Estilo
  tags$style(type = 'text/css', '#nometestedesc {white-space: pre-wrap;}'),
  
  # Título do Dashboard
  title = "Dashboard com Shiny Para Automação de Testes Estatísticos",
  tabPanel(
    title = "Tela de Início",
    sidebarLayout(
      
      # Painel lateral
      sidebarPanel(
        width = 4,
        
        # Input
        selectInput(
          inputId = "nometeste",
          label = "Selecione o Teste Estatístico Desejado:",
          choices = c("Teste t Para Uma Amostra", 
                      "Teste t Para Duas Amostras", 
                      "Teste de Wilcoxon Signed Rank", 
                      "Teste de Shapiro-Wilk",
                      "Teste Kolmogorov-Smirnov"
          ),
          selected = "Teste t Para Uma Amostra"
        ),
        textInput(
          inputId = "primeira_amostra",
          label = "Digite uma lista de valores numéricos (separados por vírgula) ou use o botão para gerar dados randômicos:"
        ),
        uiOutput("vector"),
        h5(
          actionButton(
            inputId = "randomnum",
            label = "Gerar Dados Randômicos"
          ),
          align = "center"
        ),
        uiOutput("samplemean"),
        uiOutput("confidencelevel"),
        h5(
          actionButton(
            inputId = "generate",
            label = "Executar o Teste"
          ),
          align = "center"
        )
      ),
      
      # Painel principal
      mainPanel(
        fluidRow(
          column(
            width = 6, 
            h4(textOutput("testresulttitle")), 
            withSpinner(DTOutput("testresult"), type = 7), 
            align = "center"
          ),
          column(
            width = 6, 
            h4(textOutput("histogramtitle")), 
            withSpinner(plotlyOutput("hist", width = "100%"), type = 7), 
            align = "center"
          )
        ),
        fluidRow(br()),
        fluidRow(h4(textOutput("descriptiontitle")), align = "center"),
        fluidRow(
          withSpinner(verbatimTextOutput("nometestedesc"), type = 7)
        )
      )
    )
  ),
  #nav_item(a(href = "www.ColoqueSeuEmailParaSuporte.com "Suporte"))
)

# Server

# Cria a função server
server <- function(input, output, session) {
  
  # Tema do shiny
  thematic::thematic_shiny()
  
  # Gera 20 números randômicos seguindo uma distribuição normal
  randomnumx <- eventReactive(input$randomnum, {randomnum <- rnorm(n = 20)})
  
  # Gera 20 números randômicos seguindo uma distribuição normal
  randomnumy <- eventReactive(input$randomnum, {randomnum <- rnorm(n = 20)})
  
  # Vetor dos testes de uma amostra
  output$vector <- renderUI({
    onevector <- c("Teste t Para Uma Amostra", "Teste de Wilcoxon Signed Rank", "Teste de Shapiro-Wilk")
    
    # Se o teste selecionado não estiver na lista anterior, mostra a caixa para a segunda amostra
    if(!input$nometeste %in% onevector){
      textInput(
        inputId = "segunda_amostra",
        label = "Digite a segunda lista de valores numéricos (separados por vírgula) ou use o botão para gerar dados randômicos:"
      )
    }
  })
  
  # Vetor dos testes que requerem a média amostral
  output$samplemean <- renderUI({
    samplemean <- c("Teste t Para Uma Amostra", "Teste de Wilcoxon Signed Rank", "Teste t Para Duas Amostras")
    
    # Se o teste selecionado estiver na lista anterior, mostra a caixa solicitando a média da amostra
    if(input$nometeste %in% samplemean){
      numericInput(
        inputId = "mu",
        label = "Média da Amostra",
        value = 0
      )
    }
  })
  
  # Vetor dos testes que requerem intervalo de confiança
  output$confidencelevel <- renderUI({
    confidencelevel <- c("Teste t Para Uma Amostra", "Teste de Wilcoxon Signed Rank", "Teste t Para Duas Amostras")
    
    # Se o teste selecionado estiver na lista anterior, mostra a caixa solicitando o intervalo de confiança
    if(input$nometeste %in% confidencelevel){
      selectInput(
        inputId = "conf.level",
        label = "Selecione o Intervalo de Confiança:",
        choices = list("90%" = 0.90, "95%" = 0.95, "99%" = 0.99),
        selected = 0.90
      )
    }
  })
  
  # Primeira amostra de  dados
  observe({updateTextInput(session, "primeira_amostra", value = paste(randomnumx(), collapse = ", "))})
  
  # Segunda amostra de dados
  observe({updateTextInput(session, "segunda_amostra", value = paste(randomnumy(), collapse = ", "))})
  
  # Validação das amostras
  iv <- InputValidator$new()
  iv$add_rule("primeira_amostra", sv_required())
  iv$add_rule("segunda_amostra", sv_required())
  iv$add_rule("primeira_amostra",function(value) {
    if(is.na(sum(as.numeric(unlist(str_split(value, pattern = ",")))))) {
        "Os dados devem ser numéricos e separados por vírgula"
      }
  })
  iv$add_rule("segunda_amostra", function(value) {
    if(is.na(sum(as.numeric(unlist(str_split(value, pattern = ",")))))) {
        "Os dados devem ser numéricos e separados por vírgula"
      }
    })
  
  iv$enable()
  
  # Valida as amostras
  observe({
    onevector <- c("Teste t Para Uma Amostra", "Teste de Wilcoxon Signed Rank", "Teste de Shapiro-Wilk")
    if (input$nometeste %in% onevector) {
      shinyjs::toggleState("generate", !is.null(input$primeira_amostra) && input$primeira_amostra != "")
    } else {
      shinyjs::toggleState(
        "generate", 
        !is.null(input$primeira_amostra) && input$primeira_amostra != ""
        && !is.null(input$segunda_amostra) && input$segunda_amostra != ""
      )
    }
  })
  
  # Executa o teste estatístico
  stat_test <- eventReactive(input$generate, {
    
    # Dados
    primeira_amostra <- as.numeric(unlist(str_split(input$primeira_amostra, pattern = ",")))
    segunda_amostra <- as.numeric(unlist(str_split(input$segunda_amostra, pattern = ",")))
    conf.level <- as.numeric(input$conf.level)
    
    # Executa o teste selecionado
    if(input$nometeste == "Teste t Para Uma Amostra") {
      test_result <- t.test(primeira_amostra, mu = input$mu, conf.level = conf.level) %>% tidy() 
    } 
    else if (input$nometeste == "Teste t Para Duas Amostras") {
      test_result <- t.test(x = primeira_amostra, y = segunda_amostra, mu = input$mu, conf.level = conf.level) %>% tidy()
    } 
    else if (input$nometeste == "Teste de Wilcoxon Signed Rank") {
      test_result <- wilcox.test(primeira_amostra, mu = input$mu, conf.level = conf.level) %>% tidy()
    } 
    else if (input$nometeste == "Teste de Shapiro-Wilk") {
      test_result <- shapiro.test(primeira_amostra) %>% tidy()
    } 
    else if (input$nometeste == "Teste Kolmogorov-Smirnov") {
      test_result <- ks.test(x = primeira_amostra, y = segunda_amostra) %>% tidy()
    } 
    
      # Organiza o resultado do teste
      test_result_tidy <- test_result %>% 
        mutate(result = ifelse(p.value <= 0.05, "Estatisticamente Significante, Rejeitamos a H0", 
                                                "Estatisticamente Insignificante, Falhamos em Rejeitar a H0")) %>% 
        t() %>% 
        tibble(Parameter = rownames(.), Value = .[,1]) %>% 
        select(-1) %>% 
        mutate(Parameter = str_to_title(Parameter))
    
    return(test_result_tidy)
    
  })
  
  # Tabela de resultado do teste
  output$testresult <- renderDT({
    datatable(
      stat_test(),
      rownames = FALSE,
      options = list(
        dom = 't',
        columnDefs = list(
          list(
            className = "dt-center",
            targets = "_all"
          )
        )
      )
    )
  })
  
  # Prepara os dados para o histograma
  hist_vector <- eventReactive(input$generate, {
    
    # Função densidade
    primeira_amostra <- density(as.numeric(unlist(str_split(input$primeira_amostra, pattern = ","))))
    
    return(primeira_amostra)
    
  })
  
  # Plot do histograma
  output$hist <- renderPlotly({
    hist_vector <- hist_vector()
    plot_ly(x = ~hist_vector$x, 
            y = ~hist_vector$y, 
            type = "scatter", 
            mode = "lines", 
            fill = "tozeroy") %>%  
    layout(xaxis = list(title = "Dados"), 
           yaxis = list(title = "Densidade"))
  })

  # Gera os resultados
  testresulttitle <- eventReactive(input$generate, {"Resultado do Teste"})
  histogramtitle <- eventReactive(input$generate, {"Histograma"})
  output$testresulttitle <- renderText({paste(testresulttitle())})
  output$histogramtitle <- renderText({paste(histogramtitle())})
  testdescription <- eventReactive(input$generate, {"Descrição do Teste e Hipótese Nula (H0)"})
  output$descriptiontitle <- renderText({paste(testdescription())})
  
  nometestedesc <- eventReactive(input$generate, {
    nometestedesc <- dados_desc_te %>% 
      dplyr::filter(nometeste == input$nometeste) %>% 
      dplyr::select(desc)
  })
  
  output$nometestedesc <- renderText({paste(nometestedesc())})
  
}

# Executa a app
shinyApp(ui = ui, server = server)
