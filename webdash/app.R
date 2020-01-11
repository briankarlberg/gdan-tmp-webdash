library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(iheatmapr)

library(dplyr)
library(gripql)

source("helpers.R")

cancers <- getCancers()
preds_df <- readr::read_csv("/Users/strucka/Projects/gdan-tmp/webdash/acc_predictions.csv") %>%
    dplyr::mutate(
        model_id = stringr::str_replace(model_id,
                                        sprintf("Model:%s:", "ACC"),
                                        ""),
        predicted_value = as.factor(
            stringr::str_replace(predicted_value,
                                 "Subtype:",
                                 "")
        ),
        actual_value = as.factor(
            stringr::str_replace(actual_value,
                                 "Subtype:",
                                 "")
        )
    )

ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title = "GDAN TMP"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Models", tabName = "predictions"),
            menuItem("Features", tabName = "features")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "predictions",
                h1("Compare Models"),
                # Cancer Selection
                fluidRow(
                    box(
                        width = 12,
                        selectInput(inputId = "cancer_selection", label = "Cancer Type", choices = cancers)
                    ),
                ),
                # Heatmap
                fluidRow(
                    box(
                        width = 12,
                        br(),
                        withSpinner(iheatmaprOutput("predHeatmap", height = "800px")),
                        br()
                    )
                ),
                h1("Sample Predictions Accross Repeat Folds"),
                # Sample Selection
                fluidRow(
                    box(
                        width = 12,
                        uiOutput("sampleSelection")
                    )
                ),
                # Sample Selection
                fluidRow(
                    box(
                        width = 12,
                        withSpinner(plotlyOutput('samplePredDetails'))
                    )
                )
            ),
            tabItem(
                tabName = "features",
                h1("Features")
            )
        )
    )
)

server <- function(input, output) {
    # preds_df <- getPredictions(input$cancer_selection)
    nsubtypes <- length(levels(preds_df$actual_value))
    colors <- viridis::viridis(nsubtypes)
    col_map <- as.list(colors)
    names(col_map) <- levels(preds_df$actual_value)

    output$sampleSelection <- renderUI({
        selectizeInput(inputId = "sample_selection",
                       label = "Select Sample(s)",
                       choices = unique(preds_df$sample_id),
                       multiple = T)
    })

    output$samplePredDetails <- renderPlotly({
        if (length(input$sample_selection) > 0) {
            df_subset <- preds_df %>%
                filter(sample_id %in% input$sample_selection)
            g <- ggplot(df_subset, aes(x = model_id)) +
                geom_bar(aes(fill = predicted_value)) +
                scale_fill_manual(values=c(as.character(col_map), "grey")) +
                labs(y = "Subtype Prediction by Repeat Fold", x = "", fill = "") +
                scale_y_continuous(expand = c(0, 0.1)) +
                coord_flip() +
                theme_minimal() +
                theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      axis.ticks.y = element_blank()) +
                facet_grid(. ~ sample_id)
            ggplotly(g)
        } else {
            g <- ggplot(data.frame(x = 1, y = 1, z = "Select one or more Samples"),
                   aes(x, y)) +
                geom_text(aes(label = z), size = 10) +
                theme_void()
            ggplotly(g)
        }
    })

    output$predHeatmap <- renderIheatmap({
        avg_pred <- preds_df %>%
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

        # freq_pred_wide <- avg_pred %>%
        #     dplyr::select(-n, -predicted_value) %>%
        #     tidyr::spread(sample_id, freq)
        # freq_mat <- freq_pred_wide %>% dplyr::select(-model_id) %>% as.matrix()
        # row.names(freq_mat) <- freq_pred_wide$model_id

        # subtype annotation
        subtypes <-  preds_df %>%
            dplyr::select(sample_id, actual_value) %>%
            dplyr::distinct() %>%
            dplyr::arrange(base::match(sample_id, colnames(pred_mat))) %>%
            dplyr::pull(actual_value)

        tpr_model <- preds_df %>%
            group_by(model_id) %>%
            summarize(correct = table(as.numeric(predicted_value) == as.numeric(actual_value))["TRUE"],
                      total = n()) %>%
            mutate(tpr = correct / total) %>%
            arrange(base::match(model_id, rownames(pred_mat))) %>%
            pull(tpr)

        tpr_sample <- preds_df %>%
            group_by(sample_id) %>%
            summarize(correct = table(as.numeric(predicted_value) == as.numeric(actual_value))["TRUE"],
                      total = n()) %>%
            mutate(tpr = correct / total) %>%
            arrange(base::match(sample_id, colnames(pred_mat))) %>%
            pull(tpr)

        colClust <- order.dendrogram(as.dendrogram(hclustfunc(t(pred_mat))))
        rowClust <- order.dendrogram(as.dendrogram(hclustfunc(pred_mat)))

        main_heatmap(pred_mat,
                     colors = colors,
                     show_colorbar = T,
                     col_order = colClust,
                     row_order = rowClust) %>%
            add_col_groups(subtypes,
                           name = "Subtypes",
                           title = "Actual Subtype",
                           side = "top",
                           colors = colors) %>%
            add_col_title("Samples", font = list(size = 20)) %>%
            add_row_title("Models", font = list(size = 20)) %>%
            add_col_plot(y =  tpr_sample, layout = list(title = "TPR")) %>%
            add_row_plot(x =  tpr_model, layout = list(title = "TPR"))
    })
}

shinyApp(ui = ui, server = server)
