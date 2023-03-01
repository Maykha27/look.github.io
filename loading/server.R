server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })

  #résume statistique
  output$summary <- renderPrint({
    summary()
  })


  output$messageMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })

    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "messages", .list = msgs)
  })

  myData <- reactive({
    inFile <- input$dataFile
    if (is.null(inFile)) {
      d <- return(input$dataFile$datapath)
    } else {
      d <- read.csv(input$dataFile$datapath,
                    header = as.logical(input$header),
                    sep = input$sep,
                    quote = input$quote)
    }
    d
  })
  base3 <- reactive({
    read.csv(input$dataFile$datapath,
                      header = as.logical(input$header),
                      sep = input$sep,
                      quote = input$quote)

    })



# observe({
#   if(is.null(input$dataFile)){
#     return(NULL)
#   }
#   updateSelectInput(session, 'var', label = "Test",
#                     choices = base3)
#   # updateSelectInput(session, 'var2', label = "Test22",
#   #                   choices = colnames(base3()))
#   })


  output$khady <-  renderDT({
    base3()
  })

  #Résumé statistique
  output$summary <- renderPrint({
    summary(base3())
  })

  # histogramme
  output$hist <- renderPlot({
    input$Valider
    # hist(base3()[, input$var], main = 'Histogramme', xlab = input$var)
    x    <- base3()[, input$var]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = input$var,
         main = 'Histogramme')
  })

  # Diagremme à bandes
  output$D_Bande <- renderPlotly({

    p <-  ggplot(data = base3(), aes( base3()[, input$var], base3()[, input$var3])) +
      geom_col(fill = input$couleurs) + labs(y = "", x = "")

    ggplotly(p)

  })


  # Courbes
  output$D_Line <- renderPlotly({

    pp <-  ggplot(data = base3(), aes( base3()[, input$var], base3()[, input$var4])) +
      geom_line() + labs(y = "", x = "")

    ggplotly(pp)

  })


  # Camembert
  output$Cam <- renderPlot({

    bp<- ggplot(data = base3(), aes(x="", y=as.character(base3()[, input$var]), fill=base3()[, input$var5]))+
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)
    bp

  })

  #sauvegarde de df au format csv
  output$save_data <- downloadHandler(
    filename <- function(){
      paste("data_", Sys.Date(),".csv", sep = ',')
    },
    content <- function(file){
      write.csv(df(), file)
    }
  )

  # Nuage de points
  output$nuage <- renderPlot({
    # base <- read.csv(input$dataFile$datapath,
    #                  header = as.logical(input$header),
    #                  sep = input$sep,
    #                  quote = input$quote
    # )
    plot(base3()[, input$var], base3()[, input$var2],
         xlab = input$var, ylab = input$var2)
  })



  output$preview <-  renderDataTable({

    req(input$dataFile)

    df <- read.csv(input$dataFile$datapath,
                   header = as.logical(input$header),
                   sep = input$sep,
                   quote = input$quote,
                   nrows=4000
    )
  },  options = list(scrollX = TRUE , dom = 't'))

  observe({
    updateSelectInput(session, "var",
                      label = "choisie une variable :",
                      choices = colnames(myData()))

    updateSelectInput(session, "var2",
                      label = "choisie une 2è variable :",
                      choices = colnames(myData()))

    updateSelectInput(session, "var3",
                      label = "choisie une 2è variable :",
                      choices = colnames(myData()))

    updateSelectInput(session, "var4",
                      label = "choisie une 2è variable :",
                      choices = colnames(myData()))

    updateSelectInput(session, "var5",
                      label = "choisie La variable de Groupe :",
                      choices = colnames(myData()))
  })

}
# Run the application
shinyApp(ui = ui, server = server)
