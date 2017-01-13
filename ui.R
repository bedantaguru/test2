


shinyUI(fluidPage(

  # Application title
  titlePanel("RBI Phone Book"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      select2Input("in_query","Search Here (few initial letters)",choices="",selected=c("")),
      uiOutput("ui_further_drill")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      dataTableOutput("out_contacts")
    )
  )
))
