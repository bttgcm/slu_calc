# Load required packages
library(shiny)
library(shinylive)
library(kableExtra)

# Define the UI (User Interface)
ui <- fluidPage(
  tags$img(src = "slu_logo.jpg", width = 100, alt = ""),
  titlePanel("Urea Dosing Calculator"),
  sidebarLayout(
    sidebarPanel(
      h3("Sludge characteristics:"),
      numericInput("pH", "pH", value = 9, min = 1, max = 14, step=0.1),
      numericInput("dry", "Drymatter g/L", value = 5, min = 0, max = 100),
      numericInput("temp", "Storage temperature (Celsius)", value = 10, min = 0, max = 50),
      h3("Treatment characteristics:"),
      numericInput("urea", "Applied Urea (kg/m^3)", value = 10, min = 0, max = 100),
      numericInput("va_vol", "VA Volume (m3)", value = 1000, min = 0, max = 10000),
      h3("Target application:"),
      numericInput("n_target", "Nitrogen target in the soil (kg/ha)", value = 60, min = 0, max = 100)
    ),
    mainPanel(
      uiOutput("resultsTable"),
      tags$br(),
      actionButton("printBtn", "Print Table"),
      tags$script(HTML("
        document.getElementById('printBtn').onclick = function() {
          var divToPrint = document.getElementById('resultsTable');
          var newWin = window.open('');
          newWin.document.write('<html><head><title>Print Table</title></head><body>');
          newWin.document.write(divToPrint.innerHTML);
          newWin.document.write('<p style=\"margin-top:20px; font-size:16px; color:#333;\">Add text with project link, description, credits, etc.</p>');
          newWin.document.write('</body></html>');
          newWin.document.close();
          newWin.print();
        };
      "))
    )
  )
)

# Define the Server logic
server <- function(input, output) {
  
  # Reactive calculations
  result <- reactive({
    pKa <- (2729.92/(input$temp+273.15))+0.090181
    f_nh3 <- 1/(10^(pKa-input$pH)+1)
    TAN <- (input$dry*0.145+input$urea*0.466)
    NH3aq <- TAN*f_nh3
    duration <- -5/(-0.8*((NH3aq/14.01)*1000)*0.0037*exp(0.066*input$temp))
    C_n <- input$dry *0.171 + input$urea *0.466
    application <- input$n_target/C_n
    land <- input$va_vol/application
    
    data.frame(
      Parameter = c("pH", "Drymatter (g/L)", "Storage Temperature (°C)", 
                    "Applied Urea (kg/m³)", "VA Volume (m³)", 
                    "Nitrogen Target (kg/ha)", "pKa", "f_nh3", 
                    "TAN", "NH3aq", "Duration (days)", 
                    "Application Rate (m³/ha)", "Land Extension (ha)"),
      Value = c(input$pH, input$dry, input$temp, input$urea, 
                input$va_vol, input$n_target,
                round(pKa,2), round(f_nh3,2), round(TAN,2), 
                round(NH3aq,2), round(duration,1), 
                round(application,1), round(land,1)),
      Description = c("Acidity/alkalinity of the sludge",
                      "Solid content in the sludge",
                      "Temperature during storage",
                      "Amount of urea added",
                      "Volume of value-added product",
                      "Desired nitrogen in soil",
                      "Acid dissociation constant",
                      "Fraction of ammonia",
                      "Total Ammoniacal Nitrogen",
                      "Aqueous ammonia concentration",
                      "Time required for treatment",
                      "Volume of fertilizer per hectare",
                      "Total area that can be fertilized")
    )
  })
  
  
  # Render table output using kableExtra
  output$resultsTable <- renderUI({
    result() %>%
      kbl(caption = "<span style='font-size:24px; font-weight:bold;'>Urea Dosing Calculation Results</span>") %>%
      kable_material(c("striped", "hover"), full_width = FALSE) %>%
      kable_styling(latex_options = "striped")%>%
      column_spec(1, bold = TRUE) %>%
      column_spec(2, color = "blue") %>%
      column_spec(3, italic = TRUE) %>%
      HTML()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
