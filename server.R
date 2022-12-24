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
      insertTab(inputId = "innerTabs",
                tab = tabPanel("Upload", 
                               value = "uploadTab",
                               uploaderUI("uploader")),
                target = "viewDataTab",
                position = "after"
      )
      insertTab(inputId = "innerTabs",
                tab = tabPanel("Delete Data", 
                               value = "deleteTab",
                               deleteDataUI("deleteData")),
                target = "uploadTab",
                position = "after"
      )
      insertTab(inputId = "innerTabs",
                tab = tabPanel("Edit Site", 
                               value = "editSiteTab",
                               editSiteUI("editSite")),
                target = "deleteTab",
                position = "after"
      )
    }
    else {
      removeTab(inputId = "mainTabs", target = "createSiteTab")
      removeTab(inputId = "innerTabs", target = "uploadTab")
      removeTab(inputId = "innerTabs", target = "deleteTab")
      removeTab(inputId = "innerTabs", target = "editSiteTab")
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
      updateTabsetPanel(inputId = "innerTabs", selected = "viewDataTab")
      updateTabsetPanel(inputId = "outerTabs", selected = "sitePicker")
    }
  })
}