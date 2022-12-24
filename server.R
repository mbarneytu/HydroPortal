library(leaflet)
library(shinyFeedback)

server <- function(input, output, session) {
  
  credentials <- userLoginServer("login")

  observeEvent(credentials(), {
    if (credentials()$user_auth) {
      insertTab(inputId = "mainTabs", 
                tab = tabPanel("Create new site", 
                               value = "createSiteTab",
                               createSiteUI("createSite")),
                target = "selectSiteTab",
                position = "after"
      )
    }
    else {
      removeTab(inputId = "mainTabs", target = "createSiteTab")
    }
  })
  
  createSiteServer("createSite", gageSites)
  
  # Load all sites from the database
  gageSites <- reactiveVal(loadSites())
  
  # Store the user's currently-selected site in a reactive
  selectedSite <- sitePickerServer("sitePicker", gageSites)
  
  observeEvent(selectedSite(), {
    updateTabsetPanel(inputId = "outerTabs", selected = "siteDataView")
  })

  output$siteName <- renderText({
    as.character(gageSites() |> 
                   filter(site_id == selectedSite()) |> 
                   select(site_name)
                 )
  })
  
  observeEvent(selectedSite(), {
    dataViewerServer("dataViewer", selectedSite)
  })
  
  uploaderServer("uploader", selectedSite)
  
  deleteDataServer("deleteData", selectedSite)

  observeEvent(input$innerTabs, {
    if (input$innerTabs == "selectSite") {
      updateTabsetPanel(inputId = "innerTabs", selected = "View")
      updateTabsetPanel(inputId = "outerTabs", selected = "sitePicker")
    }
  })
}