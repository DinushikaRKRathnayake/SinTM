# ===============================================================================================================
# Activate libraries
# ===============================================================================================================
source("init_libraries.R")

# ===============================================================================================================
# Initialize variables
# ===============================================================================================================
source("init_parameters.R")

# ===============================================================================================================
# Define UI for app
# ===============================================================================================================

function() {history.go(0)}

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Topic Modelling for Sinhala", titleWidth = "500px"),
                    dashboardSidebar(disable = TRUE),
                    
                    dashboardBody(
                      
                      fluidRow(
                        box(
                          fileInput(
                            inputId = "file",
                            label = "Text document:",
                            multiple = FALSE,
                            accept = c('text','text/plain')
                          ),
                          hidden(
                            div(
                              id="divErrorMsg", 
                              textOutput(outputId = "fileErrorMsg")
                            )
                          )
                        )
                      ),
                      
                      
                      fluidRow(
                        tabBox(
                          id = "tabset",width = 12,
                          
                          #__________________________________________________________________________________________________TAB 01
                          tabPanel(
                            "SinTM",
                            
                            fluidRow(
                              box(
                                width = 3,
                                title = "Inputs", status = "primary", solidHeader = TRUE, 
                                
                                strong("Params for Word cloud"),
                                tags$hr(),
                                br(),
                                
                                numericInput(
                                  inputId = "numword", 
                                  label = "Maximum number of words:",
                                  value = 100, 
                                  min = 5
                                ),
                                sliderInput(
                                  inputId = "fontsize",
                                  label = "Font Size:",
                                  min = 0,
                                  max = 1,
                                  value = 0.5
                                ),
                                
                                br(),
                                strong("Params for Model"),
                                tags$hr(),
                                br(),
                                
                                numericInput(
                                  inputId = "numtopic", 
                                  label = "Number of topics:",
                                  min = 2,
                                  value = 5
                                ),
                                numericInput(
                                  inputId = "numtopterms", 
                                  label = "Top terms:",
                                  min = 2,
                                  value = 5
                                )
                              ),
                              box(
                                width = 4,
                                title = "Word Cloud", status = "primary", solidHeader = TRUE, 
                                
                                # Output: word cloud 
                                wordcloud2Output(outputId = "plotWordCloud", height = 500)
                              ),
                              box(
                                width = 5, 
                                title = "Best Number of Topics", status = "primary", solidHeader = TRUE, 
                                
                                # Output: best number of k  
                                plotOutput("plotBestK", height = 500)
                              )
                            ),
                            
                            tags$hr(),
                            
                            fluidRow(
                              box(
                                width = 5,
                                title = "Topics", status = "primary", solidHeader = TRUE,
                                
                                # Output: topics 
                                DT::dataTableOutput(outputId = "plotDocTopics", height = 700)
                              ),
                              
                              box(
                                width = 7, 
                                title = "Diagnosis", status = "primary", solidHeader = TRUE, 
                                
                                
                                plotOutput("plotDiagnosis", height = 700)
                              )
                            )
                          ),
                          
                          #__________________________________________________________________________________________________TAB 02
                          tabPanel(
                            "Topic Models",
                            
                            
                            fluidRow(
                              box(width = 4, background = "light-blue", "LDA Base Model"),
                              box(width = 4, background = "light-blue", "LDA Base Model with Stopwords Removel"),
                              box(width = 4, background = "light-blue", "LDA Base Model with RAKE Feature")
                            ),
                            fluidRow(
                              box(
                                width = 4,
                                numericInput(
                                  inputId = "numword1", 
                                  label = "Maximum number of words:",
                                  value = 100, 
                                  min = 5
                                ),
                                sliderInput(
                                  inputId = "fontsize1",
                                  label = "Font Size:",
                                  min = 0,
                                  max = 1,
                                  value = 0.5
                                ),
                                numericInput(
                                  inputId = "numtopic1", 
                                  label = "Number of topics:",
                                  min = 2,
                                  value = 5
                                ),
                                numericInput(
                                  inputId = "numtopterms1", 
                                  label = "Top terms:",
                                  min = 2,
                                  value = 5
                                )
                              ),
                              box(
                                width = 4,
                                numericInput(
                                  inputId = "numword2", 
                                  label = "Maximum number of words:",
                                  value = 100, 
                                  min = 5
                                ),
                                sliderInput(
                                  inputId = "fontsize2",
                                  label = "Font Size:",
                                  min = 0,
                                  max = 1,
                                  value = 0.5
                                ),
                                numericInput(
                                  inputId = "numtopic2", 
                                  label = "Number of topics:",
                                  min = 2,
                                  value = 5
                                ),
                                numericInput(
                                  inputId = "numtopterms2", 
                                  label = "Top terms:",
                                  min = 2,
                                  value = 5
                                ),
                                fileInput(
                                  inputId = "wordfile",
                                  label = "Text document of stop words:",
                                  multiple = FALSE,
                                  accept = c('.txt','text/plain')
                                ),
                              ),
                              box(
                                width = 4,
                                numericInput(
                                  inputId = "numword3", 
                                  label = "Maximum number of words:",
                                  value = 100, 
                                  min = 5
                                ),
                                sliderInput(
                                  inputId = "fontsize3",
                                  label = "Font Size:",
                                  min = 0,
                                  max = 1,
                                  value = 0.5
                                ),
                                numericInput(
                                  inputId = "numtopic3", 
                                  label = "Number of topics:",
                                  min = 2,
                                  value = 5
                                ),
                                numericInput(
                                  inputId = "numtopterms3", 
                                  label = "Top terms:",
                                  min = 2,
                                  value = 5
                                )
                              )
                            ),
                            
                            fluidRow(
                              box(width = 4, title = "Word Cloud", status = "primary", wordcloud2Output(outputId = "plotWordCloudLda")),
                              box(width = 4, title = "Word Cloud", status = "primary", wordcloud2Output(outputId = "plotWordCloudLdaPos")),
                              box(width = 4, title = "Word Cloud", status = "primary", wordcloud2Output(outputId = "plotWordCloudLdaRake"))
                            ),
                            
                            br(),
                            
                            fluidRow(
                              box(width = 4, title = "Topics", status = "primary", DT::dataTableOutput(outputId = "plotDocTopicsLda")),
                              box(width = 4, title = "Topics", status = "primary", DT::dataTableOutput(outputId = "plotDocTopicsLdaPos")),
                              box(width = 4, title = "Topics", status = "primary", DT::dataTableOutput(outputId = "plotDocTopicsLdaRake"))
                            ),
                            
                            fluidRow(
                              box(width = 4, title = "Diagnosis", status = "primary", 
                                  textOutput(outputId = "coherenceLda"),
                                  textOutput(outputId = "rsquareLda")
                              ),
                              
                              box(width = 4, title = "Diagnosis", status = "primary", 
                                  textOutput(outputId = "coherenceLdaPos"),
                                  textOutput(outputId = "rsquareLdaPos")
                              ),
                              
                              box(width = 4, title = "Diagnosis", status = "primary",
                                  textOutput(outputId = "coherenceLdaRake"),
                                  textOutput(outputId = "rsquareLdaRake")
                              ),
                            ),
                            
                            
                            
                            br()
                          )
                        )
                      )
                    )
)

# ===============================================================================================================
# Define server logic
# ===============================================================================================================
server <- function(input, output) {
  
  #___________________________________________________________________________________________________Read inputs 
  # input text file data at tab 01
  data_source <- reactive({
    input_file_data <- input_file()
    return(input_file_data)
  })
  
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
   
    }else{
      validate(
        need(tolower(tools::file_ext(input$file$datapath)) == 'txt', "File type not support")
      )
      
      readLines(input$file$datapath,encoding = "UTF-8")
    }
  })
  
  # input stop word file data at tab 02
  remove_data_source <- reactive({
    input_word_file_data <- input_word_file()
    return(input_word_file_data)
  })
  
  input_word_file <- reactive({
    if (is.null(input$wordfile)) {
      return("")
    }else if (tolower(tools::file_ext(input$file$datapath)) != 'txt'){
      return("")
    }else{
      readLines(input$wordfile$datapath,encoding = "UTF-8")
    }
  })
  
  # input number of terms at tab 02
  numtopterms2 <- reactive({input$numtopterms2})
  
  # input number of words at tab 02
  numword2 <- reactive({input$numword2})
  
  #_________________________________________________________________________________________________Process Model
  # generate base model: LDA 
  source("topic_model_v1.R")
  
  model_v1_input <- reactive({
    model_v1_output <- CreateLdaModel(data_source(), input$numtopic1)
    return(model_v1_output)
  })
  
  # generate model: LDA with stopwords removal
  source("topic_model_v2.R")
  
  model_v2_input <- reactive({
    model_v2_output <- CreateLdaPosModel(data_source(), remove_data_source(), input$numtopic2)
    return(model_v2_output)
  })
  
  # generate SinTM model: LDA with RAKE
  source("topic_model_v3.R")
  
  model_v3_input <- reactive({
    model_v3_output <- CreateLdaRakeModel(data_source(), input$numtopic3)
    return(model_v3_output)
  })
  
  #___________________________________________________________________________________________Load resuorce files
  
  source("generate_wordclouds.R")
  source("generate_topics.R")
  source("topic_model.R")
  source("topic_model_bestk.R")
  source("topic_model_diagnosis.R")
  
  #____________________________________________________________________________________________Set outputs TAB 01
  
  # word cloud
  output$plotWordCloud <- renderWordcloud2({
    createWordCloudLdaRake(data_source(), num_words = input$numword, font_size = input$fontsize)
  })
  
  # best nummber of k
  output$plotBestK <- renderPlot({
    BestNumTopics(data_source())
  })
  
  # top terms(topics)
  output$plotDocTopics <- DT::renderDataTable({
    DT::datatable(
      createTopics(LdaRakeModel(data_source(), input$numtopic),input$numtopterms),
      options = list(scrollX = TRUE))
  })
  
  # diagnosis plots: loglikelyhood, coherence, prevalence
  output$plotDiagnosis <-renderPlot({
    plot_input <- LdaRakeModel(data_source(), input$numtopic)
    
    p1 <- Loglikelihood(plot_input)
    p2 <- Coherence(plot_input)
    p3 <- Prevalence(plot_input)
    
    plot_grid(p1,p2,p3)
  })
  
  #____________________________________________________________________________________________Set outputs TAB 02
  
  # out put word cloud: LDA
  output$plotWordCloudLda <- renderWordcloud2({
    createWordCloudLda(data_source(), num_words = input$numword1, font_size = input$fontsize1)
  })
  
  # out put word cloud: LDA with POS
  output$plotWordCloudLdaPos <- renderWordcloud2({
    createWordCloudLdaPos(data_source(), remove_data_source(), num_words = input$numword2, font_size = input$fontsize2)
  })
  
  # out put word cloud: LDA with RAKE
  output$plotWordCloudLdaRake <- renderWordcloud2({
    createWordCloudLdaRake(data_source(), num_words = input$numword3, font_size = input$fontsize3)
  })
  
  
  # out put model topics: LDA
  output$plotDocTopicsLda <- DT::renderDataTable({
    DT::datatable(
      createTopics(model_v1_input(),input$numtopterms1),
      options = list(scrollX = TRUE))
  })
  
  # out put model topics: LDA with POS
  output$plotDocTopicsLdaPos <- DT::renderDataTable({
    DT::datatable(
      createTopics(model_v2_input(),input$numtopterms2),
      options = list(scrollX = TRUE))
  })
  
  # out put model topics: LDA with RAKE
  output$plotDocTopicsLdaRake <- DT::renderDataTable({
    DT::datatable(
      createTopics(model_v3_input(),input$numtopterms3),
      options = list(scrollX = TRUE))
  })
  
  
  output$coherenceLda <- renderText({ 
    mc1 <- model_v1_input()
    paste0("Coherence Score: ", mean(mc1$coherence))
  })
  output$coherenceLdaPos <- renderText({ 
    mc2 <- model_v2_input()
    paste0("Coherence Score: ", mean(mc2$coherence))
  })
  output$coherenceLdaRake <- renderText({ 
    mc3 <- model_v3_input()
    paste0("Coherence Score: ", mean(mc3$coherence))
  })
  
  output$rsquareLda <- renderText({ 
    mr1 <- model_v1_input()
    paste0("R-Square        : ", mr1$r2)
  })
  output$rsquareLdaPos <- renderText({ 
    mr2 <- model_v2_input()
    paste0("R-Square        : ", mr2$r2)
  })
  output$rsquareLdaRake <- renderText({ 
    mr3 <- model_v3_input()
    paste0("R-Square        : ", mr3$r2)
  })
  
  
}

# ===============================================================================================================
# Shiny App
# ===============================================================================================================

shinyApp(ui = ui, server = server)
