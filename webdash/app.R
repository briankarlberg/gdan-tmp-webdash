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
                        withSpinner(DT::DTOutput("modelTable"))
                    ),
                    box(
                        width = 4,
                        title = "Selected Models",
                        div(style = 'height: 403px; overflow-y: auto',
                            uiOutput("selectedModelsBox")
                        )
                    ),
                    box(
                        width = 4,
                        title = "Total Features",
                        textOutput("totalFeaturesBox")
                    ),
                    box(
                        width = 4,
                        title = "Mean TPR",
                        textOutput("classificationErrorBox")
                    ),
                    box(
                        width = 4,
                        title = "Feature Types",
                        plotlyOutput("featureTypeBox", height = 300)
                    ),
                    box(
                        width = 4,
                        title = "Feature Frequency",
                        plotlyOutput("featureFreqBox", height = 300)
                    )
                )
            ),
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
            )
        )
    )
)

# TODO: use the future and promises packages
# https://blog.rstudio.com/2018/06/26/shiny-1-1-0/
#
server <- function(input, output, session) {
    ##--------------------
    ## Global Data
    ##--------------------
    source("load_test_data.R")

    featureSets <- reactive({
        # getFeatureSets(NULL)
        feature_sets
    })

    predictions <- reactive({
        # getPredictions(NULL)
        preds
    })

    # cancers <- getCancers()
    # features <- getFeatures()

    ##--------------------
    ## Sidebar filters
    ##--------------------

    updateSelectizeInput(session = session,
                         inputId = 'cancer_selection',
                         choices = cancers,
                         server = FALSE)

    updateSelectizeInput(session = session,
                         inputId = 'feature_selection',
                         choices = features,
                         server = TRUE)

    observe({
        updateSelectizeInput(session = session,
                             inputId = 'sample_selection',
                             choices =  unique(cancer_preds()$sample_id),
                             server = TRUE)
    })

    ##----------------------
    ## Shared Reactive Vals
    ##----------------------
    cancer_preds <- reactiveVal(tibble())
    cancer_subtype_colors <- reactiveVal(c())

    observeEvent(input$cancer_selection, {
        res <- predictions() %>%
            dplyr::filter(cancer_id == input$cancer_selection) %>%
            droplevels()
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
        dplyr::left_join(
            predictions() %>%
                dplyr::group_by(cancer_id, model_id, featureset_id, type) %>%
                dplyr::summarize(correct = table(as.numeric(predicted_value) == as.numeric(actual_value))["TRUE"],
                                 total = n()) %>%
                dplyr::ungroup() %>%
                dplyr::mutate(tpr = round(correct / total, digits = 3)) %>%
                dplyr::select(cancer_id, model_id, featureset_id, type, tpr) %>%
                tidyr::spread(type, tpr) %>%
                dplyr::rename(Project = cancer_id, Model = model_id, Features = featureset_id,
                              TPR_Training = training, TPR_Testing = testing),

            featureSets() %>%
                dplyr::group_by(featureset_id, cancer_id) %>%
                dplyr::summarize(N_Features = n()) %>%
                dplyr::ungroup() %>%
                dplyr::rename(Features = featureset_id, Project = cancer_id)
        ) %>%
            dplyr::group_by(Project) %>%
            arrange(desc(TPR_Testing)) %>%
            dplyr::mutate(Model_Rank = dplyr::row_number()) %>%
            dplyr::arrange(Model_Rank, TPR_Testing) %>%
            dplyr::ungroup()
    })

    selected_models <- reactive({
        ms <- model_summary()
        which(paste(ms$Project, ms$Model_Rank, sep = "|") %in% paste(cancers, 1,  sep = "|"))
    })

    output$modelTable <- DT::renderDT({
        model_summary() %>% dplyr::select(-Model_Rank)
    },
    filter = "top",
    rownames = FALSE,
    selection = list(selected = selected_models(), target = "row"),
    options = list(
        scrollX = TRUE
    ))

    output$selectedModelsBox <- renderUI({
        if (length(input$modelTable_rows_selected) > 0) {
            selected_models <- model_summary() %>%
                dplyr::slice(input$modelTable_rows_selected) %>%
                dplyr::pull(Model) %>%
                paste(collapse = "<br/>")
            shiny::HTML(
                sprintf(
                    "<div>%s</div>",
                    selected_models
                )
            )
        }
    })

    output$totalFeaturesBox <- renderText({
        model_summary() %>%
            dplyr::slice(input$modelTable_rows_selected) %>%
            dplyr::pull(N_Features) %>%
            sum()
    })

    output$classificationErrorBox <- renderText({
        model_summary() %>%
            dplyr::slice(input$modelTable_rows_selected) %>%
            dplyr::pull(TPR_Testing) %>%
            mean()
    })

    selected_features <- reactive({
        fsets <- model_summary() %>%
            dplyr::slice(input$modelTable_rows_selected) %>%
            dplyr::pull(Features)
        projects <- model_summary() %>%
            dplyr::slice(input$modelTable_rows_selected) %>%
            dplyr::pull(Project)

        featureSets() %>%
            filter(featureset_id %in% fsets,
                   cancer_id %in% projects)
    })

    output$featureTypeBox <- renderPlotly({
        selected_features() %>%
            tidyr::separate(feature_id,
                            sep = "\\:",
                            into = c("datatype", "platform1", "platform2", "featureid1", "featureid2", "extra"),
                            remove = F) %>%
            dplyr::count(platform1) %>%
            mutate(share = n / sum(n) * 100) %>%
            arrange(desc(share)) %>%
            plot_ly(labels = ~platform1, values = ~share,  type = "pie",
                    textposition = 'inside',
                    textinfo = 'label+percent',
                    insidetextfont = list(color = '#FFFFFF', size = 15),
                    hoverinfo = 'text',
                    text = ~paste(round(share, digits = 2), '%'),
                    showlegend = FALSE)
    })

    output$featureFreqBox <- renderPlotly({
        fcount <- selected_features() %>%
            dplyr::group_by(feature_id) %>%
            dplyr::summarize(model_occurence = as.factor(n())) %>%
            dplyr::ungroup() %>%
            dplyr::group_by(model_occurence) %>%
            summarize(count = n())

        g <- ggplot(fcount, aes(model_occurence, count)) +
            geom_bar(stat = "identity") +
            scale_y_continuous(breaks = scales::pretty_breaks()) +
            labs(y = "Count", x = "Number of Models", title = "") +
            coord_flip() +
            theme_minimal(12) +
            theme(axis.text.x = element_text(face = "bold", size = 10, angle = 45),
                  legend.position = "none")
        ggplotly(g)

    })

    ##----------------------
    ## Model Predictions Tab
    ##----------------------

    output$predHeatmap <- renderIheatmap({
        avg_pred <- cancer_preds() %>%
            dplyr::filter(type == input$set_selection) %>%
            dplyr::group_by(prediction_id, sample_id) %>%
            dplyr::count(predicted_value) %>%
            dplyr::mutate(freq = n / sum(n)) %>%
            dplyr::arrange(desc(n), .by_group = TRUE) %>%
            dplyr::slice(1) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(predicted_value = as.numeric(predicted_value))

        avg_pred_wide <- avg_pred %>%
            dplyr::select(-n, -freq) %>%
            tidyr::spread(sample_id, predicted_value)

        pred_mat <- avg_pred_wide %>% dplyr::select(-prediction_id) %>% as.matrix()
        row.names(pred_mat) <- avg_pred_wide$prediction_id

        subtypes <-  cancer_preds() %>%
            dplyr::select(sample_id, actual_value) %>%
            dplyr::distinct() %>%
            dplyr::arrange(base::match(sample_id, colnames(pred_mat))) %>%
            dplyr::pull(actual_value)

        tpr_model <- cancer_preds() %>%
            dplyr::filter(type == input$set_selection) %>%
            dplyr::group_by(prediction_id) %>%
            dplyr::summarize(correct = table(as.numeric(predicted_value) == as.numeric(actual_value))["TRUE"],
                             total = n()) %>%
            dplyr::mutate(tpr = correct / total) %>%
            dplyr::arrange(base::match(prediction_id, rownames(pred_mat))) %>%
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
            add_col_groups(subtypes %>% as.character(),
                           name = "Subtypes",
                           title = "Actual Subtype",
                           side = "top",
                           colors = cancer_subtype_colors() %>% as.character()) %>%
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
        hm %>% modify_layout(list(height = 400 + 15*dim(pred_mat)[1]))
    })

    output$featureFreq <- renderPlotly({
        fset <- featureSets() %>% dplyr::filter(cancer_id  == input$cancer_selection)
        fset_sub <- dplyr::left_join(
            fset,
            fset %>% dplyr::count(feature_id, name = "count")
        ) %>%
            dplyr::select(feature_id, count, featureset_id) %>%
            dplyr::distinct() %>%
            dplyr::group_by(feature_id, count) %>%
            dplyr::summarise(feature_sets = toString(featureset_id)) %>%
            dplyr::ungroup() %>%
            dplyr::arrange(desc(count)) %>%
            dplyr::mutate(feature_id = reorder(feature_id, -count)) %>%
            dplyr::filter(count >= input$nmodels)

        if (dim(fset_sub)[1] > 0) {
            g <- fset_sub %>%
                ggplot(aes(feature_id, count)) +
                geom_bar(aes(size = feature_sets), stat = "identity") +
                scale_y_continuous(breaks = scales::pretty_breaks()) +
                labs(y = "# of Models", x = "") +
                theme_minimal(15) +
                theme(axis.text.x = element_text(face = "bold", size = 10, angle = 45),
                      legend.position = "none")
        } else {
            g <- ggplot(data.frame(x = 1, y = 1, z = sprintf("No Features Appeared >= %s Times", input$nmodels)),
                        aes(x, y)) +
                geom_text(aes(label = z), size = 10) +
                theme_void()
        }
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
            g <- ggplot(df_subset, aes(x = prediction_id)) +
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
                height = 400 + 10*length(unique(df_subset$prediction_id))*ceiling(length(input$sample_selection)/2)
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
            vals <- feature_vals %>% dplyr::filter(feature_id %in% input$feature_selection)
            feature_subset(vals)
        } else {
            feature_subset(tibble())
        }
    })

    # TODO change cancer filter to allow for multiple selections
    observeEvent(c(dim(feature_subset())[1], input$cancer_selection), {
        if (dim(feature_subset())[1] > 0) {
            vals <- dplyr::inner_join(
                predictions() %>%
                    dplyr::filter(cancer_id == input$cancer_selection) %>%
                    dplyr::select(sample_id, subtype_id = actual_value) %>%
                    dplyr::distinct(),
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
