library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(iheatmapr)
library(plotly)

library(dplyr)
library(gripql)

source("helpers.R")

ui <- dashboardPage(
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
        uiOutput("filters")
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "predictions",
                h1("Compare Models"),
                fluidRow(
                    box(
                        width = 12,
                        withSpinner(iheatmaprOutput("predHeatmap", height = "100%", width = "98%")),
                        tags$div(
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
                        tags$div(align = "center",
                                 class = "multicol",
                                 sliderInput("nmodels", "Minimum frequency:",
                                             min = 1, max = 10, step = 1, value = 2,
                                             width = "30%")),
                        withSpinner(plotlyOutput("featureFreq", height = "100%", width = "98%"))
                    ),
                ),
            ),
            tabItem(
                tabName = "samples",
                h1("Subtype Prediction Stability Across Repeat Folds"),
                fluidRow(
                    box(
                        width = 12,
                        withSpinner(plotlyOutput("samplePredDetails", height = "100%",  width = "98%"))
                    )
                )
            ),
            tabItem(
                tabName = "features",
                h1("Feature Value Distributions"),
                fluidRow(
                    box(
                        width = 12,
                        withSpinner(plotlyOutput("featureDetails", height = "100%", width = "98%"))
                    )
                ),
            ),
            tabItem(
                tabName = "models",
                # h1("Models"),
                fluidRow(
                    box(
                        width = 12,
                        withSpinner(DT::DTOutput("modelTable"))
                    ),
                    box(
                        width = 4,
                        title = "Selected Models",
                        uiOutput("selectedModelsBox")
                    ),
                    box(
                        width = 4,
                        title = "Total Features",
                        textOutput("totalFeaturesBox")
                    ),
                    box(
                        width = 4,
                        title = "Mean Classification Error",
                        textOutput("classificationErrorBox")
                    )
                )
            )
        )
    )
)

# TODO: use the future and promises packages
# https://blog.rstudio.com/2018/06/26/shiny-1-1-0/
#
server <- function(input, output) {
    ##--------------------
    ## Global Data
    ##--------------------
    source("load_test_data.R")

    featureSets <- reactive({
        # getFeatureSets(NULL)
        acc_feature_set
    })

    predictions <- reactive({
        # getPredictions(NULL)
        acc_preds
    })

    # cancers <- getCancers()
    # features <- getFeatures()

    ##--------------------
    ## Sidebar filters
    ##--------------------
    output$filters <- renderUI({
        if (input$tabs == "predictions") {
            div(
                selectInput(inputId = "cancer_selection",
                            label = "Cancer Type",
                            choices = cancers,
                            selected = input$cancer_selection),
                selectInput(inputId = "set_selection",
                            label = "Data Subset",
                            choices = c("testing", "training"),
                            selected = input$set_selection)
            )
        } else if (input$tabs == "features") {
            div(
                selectInput(inputId = "cancer_selection",
                            label = "Cancer Type",
                            choices = cancers,
                            selected = input$cancer_selection),
                selectizeInput(inputId = "feature_selection",
                               label = "Select Features(s)",
                               choices = features,
                               selected = input$feature_selection,
                               multiple = T)
            )
        } else if (input$tabs == "samples") {
            div(
                selectInput(inputId = "cancer_selection",
                            label = "Cancer Type",
                            choices = cancers,
                            selected = input$cancer_selection),
                selectInput(inputId = "set_selection",
                            label = "Data Subset",
                            choices = c("testing", "training"),
                            selected = input$set_selection),
                selectizeInput(inputId = "sample_selection",
                               label = "Select Sample(s)",
                               choices = unique(cancer_preds()$sample_id),
                               multiple = T,
                               selected = input$sample_selection)
            )
        } else {
            div()
        }
    })

    ##----------------------
    ## Shared Reactive Vals
    ##----------------------
    cancer_preds <- reactiveVal(tibble())
    cancer_subtype_colors <- reactiveVal(c())

    observeEvent(input$cancer_selection, {
        res <- predictions() %>%
            dplyr::filter(cancer_id == input$cancer_selection)
        if (dim(res)[1] > 0) {
            nsubtypes <- length(levels(res$actual_value))
            cols <- viridis::viridis(nsubtypes)
            names(cols) <- levels(res$actual_value)
            cancer_subtype_colors(cols)
        } else {
            cancer_subtype_colors(c())
        }
        cancer_preds(res)
    })

    ##--------------------
    ## Panel Builder Tab
    ##--------------------

    model_summary <- reactive({
        predictions() %>%
            dplyr::group_by(cancer_id, model_id, featureset_id, type) %>%
            dplyr::summarize(correct = table(as.numeric(predicted_value) == as.numeric(actual_value))["TRUE"],
                             total = n()) %>%
            ungroup() %>%
            dplyr::mutate(tpr = round(correct / total, digits = 3)) %>%
            dplyr::select(cancer_id, model_id, featureset_id, type, tpr) %>%
            tidyr::spread(type, tpr) %>%
            dplyr::rename(Project = cancer_id, Model = model_id, Features = featureset_id, TPR_Training = training, TPR_Testing = testing) %>%
            dplyr::mutate(N_Features = NA, GEXP = NA, MIR = NA, METH = NA, MUTA = NA, CNVR = NA) %>%
            dplyr::arrange(desc(TPR_Testing))
    })

    # selected_models <- reactive({
    #     model_summary() %>%
    #         group_by(project) %>%
    #         filter(TPR_Testing == max(TPR_Testing)) %>%
    # })

    output$modelTable <- DT::renderDT({
        model_summary()
    },
    filter = "top",
    rownames = FALSE,
    selection = list(selected = c(1, 3), target = "row"),
    options = list(
        scrollX = TRUE
    ))

    output$selectedModelsBox <- renderUI({
        if (length(input$modelTable_rows_selected) > 0) {
            selected_models <- model_summary() %>%
                dplyr::slice(input$modelTable_rows_selected) %>%
                dplyr::pull(Model) %>%
                paste(collapse = "<br/>")
            shiny::HTML(selected_models)
        }
    })

    ##----------------------
    ## Model Predictions Tab
    ##----------------------

    output$predHeatmap <- renderIheatmap({
        avg_pred <- cancer_preds() %>%
            dplyr::filter(type == input$set_selection) %>%
            dplyr::group_by(model_id, sample_id) %>%
            dplyr::count(predicted_value) %>%
            dplyr::mutate(freq = n / sum(n)) %>%
            dplyr::arrange(desc(n)) %>%
            dplyr::slice(1) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
                predicted_value = as.numeric(predicted_value)
            )

        avg_pred_wide <- avg_pred %>%
            dplyr::select(-n, -freq) %>%
            tidyr::spread(sample_id, predicted_value)

        pred_mat <- avg_pred_wide %>% dplyr::select(-model_id) %>% as.matrix()
        row.names(pred_mat) <- avg_pred_wide$model_id

        subtypes <-  cancer_preds() %>%
            dplyr::select(sample_id, actual_value) %>%
            dplyr::distinct() %>%
            dplyr::arrange(base::match(sample_id, colnames(pred_mat))) %>%
            dplyr::pull(actual_value)

        tpr_model <- cancer_preds() %>%
            dplyr::filter(type == input$set_selection) %>%
            dplyr::group_by(model_id) %>%
            dplyr::summarize(correct = table(as.numeric(predicted_value) == as.numeric(actual_value))["TRUE"],
                             total = n()) %>%
            dplyr::mutate(tpr = correct / total) %>%
            dplyr::arrange(base::match(model_id, rownames(pred_mat))) %>%
            dplyr::pull(tpr)

        tpr_sample <- cancer_preds() %>%
            dplyr::filter(type == input$set_selection) %>%
            dplyr::group_by(sample_id) %>%
            dplyr::summarize(correct = table(as.numeric(predicted_value) == as.numeric(actual_value))["TRUE"],
                             total = n()) %>%
            dplyr::mutate(tpr = correct / total) %>%
            dplyr::arrange(base::match(sample_id, colnames(pred_mat))) %>%
            dplyr::pull(tpr)

        colClust <- order.dendrogram(as.dendrogram(hclustfunc(t(pred_mat))))
        rowClust <- order.dendrogram(as.dendrogram(hclustfunc(pred_mat)))

        hm <- main_heatmap(pred_mat,
                           colors = cancer_subtype_colors(),
                           show_colorbar = FALSE,
                           col_order = colClust,
                           row_order = rowClust) %>%
            add_col_groups(subtypes,
                           name = "Subtypes",
                           title = "Actual Subtype",
                           side = "top",
                           colors = cancer_subtype_colors()) %>%
            add_col_plot(y =  tpr_sample, layout = list(title = "TPR")) %>%
            add_row_plot(x =  tpr_model, layout = list(title = "TPR"))

        if ("samples" %in% input$predHeatmap_axes) {
            hm <- hm %>% add_col_labels()
        } else {
            hm <- hm %>% add_col_title("Samples", font = list(size = 20))
        }

        if ("models" %in% input$predHeatmap_axes) {
            hm <- hm %>% add_row_labels()
        } else {
            hm <- hm %>% add_row_title("Models", font = list(size = 20))
        }
        hm %>% modify_layout(list(height = 15*dim(pred_mat)[1]))
    })

    output$featureFreq <- renderPlotly({
        g <- featureSets() %>%
            group_by(feature_id) %>%
            summarize(count = n()) %>%
            filter(count >= input$nmodels) %>%
            ungroup() %>%
            mutate(feature = reorder(feature_id, -count)) %>%
            ggplot(aes(feature_id, count)) +
            geom_bar(stat = "identity") +
            scale_y_continuous(breaks = scales::pretty_breaks()) +
            labs(y = "# of Models", x = "") +
            theme_minimal(15) +
            theme(axis.text.x = element_text(face = "bold", size = 10, angle = 45))
        ggplotly(g, height = 400)
    })

    ##------------------------
    ## Sample Predictions Tab
    ##------------------------

    ## See https://stackoverflow.com/questions/16389636/in-ggplot2-how-can-i-add-additional-legend

    output$samplePredDetails <- renderPlotly({
        if (length(input$sample_selection) > 0) {
            df_subset <- cancer_preds() %>%
                dplyr::filter(type == input$set_selection) %>%
                dplyr::filter(sample_id %in% input$sample_selection) %>%
                mutate(sample_label = paste(sample_id, actual_value, sep = " - Subtype: "))
            g <- ggplot(df_subset, aes(x = model_id)) +
                geom_bar(aes(fill = predicted_value), position = "fill") +
                scale_fill_manual(values = c(cancer_subtype_colors(), "grey")) +
                labs(y = "", x = "", fill = "") +
                scale_y_continuous(expand = expand_scale(add = c(0.01, 0.05))) +
                coord_flip() +
                theme_minimal() +
                theme(axis.ticks.y = element_blank(),
                      axis.text.y = element_text(size = 7),
                      panel.grid.major = element_blank()) +
                facet_wrap(~sample_label, ncol = 2)
            ggplotly(
                g,
                height = 10*length(unique(df_subset$model_id))*ceiling(length(input$sample_selection)/2)
            )
        } else {
            g <- ggplot(data.frame(x = 1, y = 1, z = "Select One or More Samples"),
                        aes(x, y)) +
                geom_text(aes(label = z), size = 10) +
                theme_void()
            ggplotly(g, height = 400)
        }
    })

    ##---------------------------
    ## Feature Distributions Tab
    ##---------------------------
    feature_subset <- reactiveVal(tibble())
    cancer_feature_subset <-  reactiveVal(tibble())

    observeEvent(length(input$feature_selection), {
        if (length(input$feature_selection) > 0) {
            # vals <- getFeaturesValues(input$feature_selection)
            vals <- acc_feature_vals %>% dplyr::filter(feature_id %in% input$feature_selection)
            feature_subset(vals)
        } else {
            feature_subset(tibble())
        }
    })

    # TODO change cancer filter to allow for multiple selections
    observeEvent(c(dim(feature_subset())[1], input$cancer_selection), {
        if (dim(feature_subset())[1] > 0) {
            vals <- inner_join(
                predictions() %>%
                    dplyr::filter(cancer_id == input$cancer_selection) %>%
                    dplyr::select(sample_id, subtype_id = actual_value) %>%
                    distinct(),
                feature_subset()
            )
            cancer_feature_subset(vals)
        } else {
            cancer_feature_subset(tibble())
        }
    })

    output$featureDetails <- renderPlotly({
        sub <- cancer_feature_subset()
        if (dim(sub)[1] > 0) {
            nfeatures <- length(unique(sub$feature_id))
            g <- ggplot(sub,
                        aes(x = subtype_id,
                            y = value,
                            fill = subtype_id)) +
                geom_violin(colour = NA, na.rm = T, alpha = 0.5) +
                geom_jitter(width = 0.05, height = 0.05) +
                coord_flip() +
                labs(y = "", x = "", fill = "") +
                theme_minimal() +
                theme(axis.text.y = element_blank()) +
                facet_wrap(~feature_id, ncol = 2, scales = "free_x")
            ggplotly(
                g,
                tooltip = c("x", "y"),
                height = 300*ceiling(nfeatures/2)
            )
        } else {
            g <- ggplot(data.frame(x = 1, y = 1, z = "Select One or More Features"),
                        aes(x, y)) +
                geom_text(aes(label = z), size = 10) +
                theme_void()
            ggplotly(g, height = 400)
        }
    })

}

shinyApp(ui = ui, server = server)
