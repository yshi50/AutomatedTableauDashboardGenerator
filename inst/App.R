options(shiny.maxRequestSize = 1024*1024^2)

ui <- fluidPage(

    titlePanel(h1("Automated Tableau Dashboard Generator R Shiny App",
                  align = "center")),

    sidebarLayout(
        sidebarPanel(
          h3("Required Inputs", align = "center"),
          tags$hr(style = "border-color: purple;"),
          radioButtons( 
            inputId = "source",
            label = "Select Data Source",
            choices = list(
              "Isilon Path" = "Isilon",
              "GDataQuery" = "GDataQuery",
              "Upload Files" = "Upload"
              )
          ),
          uiOutput("source_choice"),
          uiOutput("source_holder"),
          h3("Optional Inputs", align = "center"),
          tags$hr(style = "border-color: purple;"),
          textInput(
            "save_path",
            "Dashboard Output Isilon Path",
            value = getwd()
          ),
          textInput(
            "UID",
            "UID",
            value = "SUBJID"
          ),
          textInput( 
            "study_name",
            "Study Name",
          ),
          textInput( 
            "special_name",
            "Special Name to Rename"
          ),
          textAreaInput( 
            "about",
            "Study Information",
            value = "Not Available"
          ),
          textInput( 
            "URL",
            "URL"
          ),
          numericInput( 
            "log_fold_change",
            "Log-n Fold Change",
            value = NA,
            min = 2
          ),
          numericInput( 
            "reference_line",
            "Reference Line",
            value = NA
          ),
          textAreaInput( 
            "note",
            "Creator Message",
            placeholder = "No Message",
            value = "No Message"
          ),
          width = 4),

        mainPanel(
          verbatimTextOutput("console"),
          uiOutput("download")
        )
    )
)

server <- function(input, output) {
  
  observeEvent(input$start, {
    req(input$start)
    
    source = isolate(input$source)
    about = isolate(input$about)
    study_name = isolate(input$study_name)
    save_path = isolate(sub("/$", "", input$save_path))
    
    special_name = isolate(input$special_name)
    if (special_name == "")
      special_name = NA
    
    URL = isolate(input$URL)
    if (URL == "")
      URL = NA
    
    withProgress(
      min = 1, 
      max = 6, {
        setProgress(2, message = "Loading...")
        
        #####
        if (source == "Isilon") {
          
          abk_0 = isolate(input$abk_path)
          adsl_0 = isolate(input$adsl_path)

        } else if (source == "GDataQuery") {

          return()
          
        } else {
          
          abk_0 = isolate(input$abk_upload$datapath)
          adsl_0 = isolate(input$adsl_upload$datapath)

        }
        
        if (study_name == "")
          study_name = NA
        #####
        
        setProgress(3, message = "Generating...")
        
        #####
        output$console = renderText({
          Template(abk_0 = abk_0, adsl_0 = adsl_0, save_path = save_path,
                   UID = isolate(input$UID), study_name = study_name, special_name = special_name,
                   about = about, URL = URL, log_fold_change = isolate(input$log_fold_change),
                   reference_line = isolate(input$reference_line), note = isolate(input$note), shiny = TRUE)
        })
        #####
        
        setProgress(4, message = "Saving...")
        
        #####
        
        output$downloadZip <- downloadHandler(
          filename = function() {
            "output.zip"
          },
          content = function(file) {
            
            temp_zip <- tempfile(fileext = ".zip")
            
            zip(zipfile = temp_zip, files = file.path(paste0(save_path, "/", "GTableauPipeline")))
            
            file.rename(temp_zip, file)
          }
        )
        
        output$download <- renderUI({
          downloadButton("downloadZip", "Download Output as Zip File")
        })
        
        #####
        
        setProgress(5, message = "Done...")
        
        #####
      })

    })
  
  output$source_holder = renderUI({
    
    if (input$source != "GDataQuery"){
      actionButton("start", "Start Generate Dashboard")
    } else{
      renderText({"This Function Is Not Available"})
    }

  })
  
  output$source_choice = renderUI({
    if (input$source == "Isilon"){
      tagList(
        textInput( 
          "abk_path",
          "Input ABK Isilon Path",
        ),
        textInput( 
          "adsl_path",
          "Input ADSL Isilon Path",
        )
      )
    } else if (input$source == "GDataQuery"){
      
      return()
      
    } else {
      tagList(
        fileInput("abk_upload", "Upload ABK", accept = c(".csv", ".sas")),
        fileInput("adsl_upload", "Upload ADSL", accept = c(".csv", ".sas"))
      )
    }
  })

}

shinyApp(ui = ui, server = server)