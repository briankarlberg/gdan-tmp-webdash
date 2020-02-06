suppressMessages({
    library(shiny)
    library(shinydashboard)
    library(shinycssloaders)
    library(plotly)
    library(iheatmapr)
})

##-------------------------
## UI
##-------------------------
dashboardPage(
    skin = "black",
    dashboardHeader(title = "GDAN TMP"),
    dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            menuItem("Panel Builder", tabName = "models"),
            menuItem("Model Predictions", tabName = "predictions"),
            menuItem("Sample Predictions", tabName = "samples"),
            menuItem("Feature Distributions", tabName = "features")
        ),
        br(),
        conditionalPanel(
            "input.tabs != 'models'",
            selectizeInput(inputId = "cancer_selection",
                           label = "Cancer Type",
                           choices = NULL)
        ),
        conditionalPanel(
            "input.tabs == 'predictions' || input.tabs == 'samples'",
            selectizeInput(inputId = "set_selection",
                           label = "Data Subset",
                           choices = c("testing", "training"))
        ),
        conditionalPanel(
            "input.tabs == 'samples'",
            selectizeInput(inputId = "sample_selection",
                           label = "Select Sample(s)",
                           choices = NULL,
                           multiple = T)
        ),
        conditionalPanel(
            "input.tabs == 'features'",
            selectizeInput(inputId = "feature_selection",
                           label = "Select Features(s)",
                           choices = NULL,
                           multiple = T)
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "models",
                fluidRow(
                    box(
                        width = 12,
                        div(style = 'max-height: 750px; overflow-y: auto',
                            withSpinner(DT::DTOutput("modelTable"))
                        )
                    ),
                    tabBox(
                        title = "Selected",
                        side = "right",
                        selected = "Models",
                        width = 4,
                        tabPanel(
                            "Features",
                            div(style = 'height: 439px; overflow-y: auto',
                                withSpinner(uiOutput("selectedFeaturesBox"))
                            ),
                        ),
                        tabPanel(
                            "Models",
                            div(style = 'height: 439px; overflow-y: auto',
                                withSpinner(uiOutput("selectedModelsBox"))
                            ),
                        )
                    ),
                    box(
                        width = 4,
                        title = "Total Features",
                        h3(withSpinner(textOutput("totalFeaturesBox")))
                    ),
                    box(
                        width = 4,
                        title = "Mean TPR",
                        h3(withSpinner(textOutput("classificationErrorBox")))
                    ),
                    box(
                        width = 4,
                        title = "Feature Types",
                        withSpinner(plotlyOutput("featureTypeBox", height = 300, width = "98%"))
                    ),
                    box(
                        width = 4,
                        title = "Feature Frequency",
                        withSpinner(plotlyOutput("featureFreqBox", height = 300, width = "98%"))
                    ),
                    box(
                        width = 12,
                        title = "Panel Performance - TPR x Cancer Subtype",
                        div(style = 'max-height: 750px; overflow-y: auto',
                            withSpinner(plotlyOutput("performanceBox", height = "100%", width = "98%"), proxy.height = "300px")
                        )
                    )
                )
            ),
            tabItem(
                tabName = "predictions",
                h1("Compare Models"),
                fluidRow(
                    box(
                        width = 12,
                        withSpinner(iheatmaprOutput("predHeatmap", height = "100%", width = "98%"), proxy.height = "400px"),
                        div(
                            align = "center",
                            class = "multicol",
                            shinyWidgets::prettyCheckboxGroup("predHeatmap_axes",
                                                              label = "Show Labels:",
                                                              choices = c("Models" = "models",
                                                                          "Samples" = "samples"),
                                                              inline = TRUE,
                                                              bigger = TRUE,
                                                              width = "100%")
                        )
                    )
                ),
                h1("Feature Frequency in Models"),
                fluidRow(
                    box(
                        width = 12,
                        div(align = "center",
                            class = "multicol",
                            sliderInput("nmodels", "Minimum frequency:",
                                        min = 1, max = 10, step = 1, value = 2,
                                        width = "30%")),
                        withSpinner(plotlyOutput("featureFreq", height = "400px", width = "98%"))
                    ),
                ),
            ),
            tabItem(
                tabName = "samples",
                h1("Subtype Prediction Stability Across Repeat Folds"),
                fluidRow(
                    box(
                        width = 12,
                        withSpinner(plotlyOutput("samplePredDetails", height = "100%",  width = "98%"), proxy.height = "200px")
                    )
                )
            ),
            tabItem(
                tabName = "features",
                h1("Feature Value Distributions"),
                fluidRow(
                    box(
                        width = 12,
                        withSpinner(plotlyOutput("featureDetails", height = "100%", width = "98%"), proxy.height = "200px")
                    )
                )
            )
        )
    )
)
