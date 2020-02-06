suppressMessages({
    library(shiny)
    library(iheatmapr)
    library(plotly)
    library(dplyr)
})


# TODO: use the future and promises packages
# https://blog.rstudio.com/2018/06/26/shiny-1-1-0/
function(input, output, session) {
    # message("request: ", paste(ls(env=session$request), collapse=", "))
    message("HTTP_REMOTE_USER: ", session$request$HTTP_REMOTE_USER)
    message("HTTP_REMOTE_ROLES: ", session$request$HTTP_REMOTE_ROLES)
    message("HTTP_USER_AGENT: ", session$request$HTTP_USER_AGENT)

    ##--------------------
    ## Sidebar filters
    ##--------------------

    observeEvent(input$cancer_selection, {
        if (input$cancer_selection == "") {
            return()
        }
        features <- feature_con %>%
            dplyr::tbl(sprintf("%s_features", input$cancer_selection)) %>%
            dplyr::select(feature_id) %>%
            dplyr::collect() %>%
            pull(feature_id)

        updateSelectizeInput(session = session,
                             inputId = 'feature_selection',
                             choices = features,
                             selected = input$selected_features,
                             server = TRUE)
    })

    updateSelectizeInput(session = session,
                         inputId = 'cancer_selection',
                         choices = cancers,
                         server = FALSE)



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
        res <- predictions %>%
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

    output$modelTable <- DT::renderDT({
        model_summary %>% dplyr::select(-Model_Rank, -Date)
    },
    filter = "top",
    rownames = FALSE,
    selection = list(selected = selected_models, target = "row"),
    options = list(
        scrollX = TRUE,
        columnDefs = list(list(visible=FALSE, targets=c(5)))
    ))

    output$selectedModelsBox <- renderUI({
        if (length(input$modelTable_rows_selected) == 0) {
            return()
        }
        selected <- model_summary %>%
            dplyr::slice(input$modelTable_rows_selected) %>%
            dplyr::pull(prediction_id) %>%
            paste(collapse = "<br/><br/>")
        shiny::HTML(
            sprintf(
                "<div>%s</div>",
                selected
            )
        )
    })

    output$totalFeaturesBox <- renderText({
        if (length(input$modelTable_rows_selected) == 0) {
            return()
        }
        model_summary %>%
            dplyr::slice(input$modelTable_rows_selected) %>%
            dplyr::pull(N_Features) %>%
            sum()
    })

    output$classificationErrorBox <- renderText({
        if (length(input$modelTable_rows_selected) == 0) {
            return()
        }
        model_summary %>%
            dplyr::slice(input$modelTable_rows_selected) %>%
            dplyr::pull(TPR_Testing) %>%
            mean()
    })

    selected_panel_features <- reactive({
        if (length(input$modelTable_rows_selected) == 0) {
            return()
        }
        suppressMessages({
            inner_join(
                model_summary %>%
                    dplyr::slice(input$modelTable_rows_selected) %>%
                    dplyr::select(featureset_id = Features, cancer_id = Project),
                featureSets
            )
        })
    })

    output$selectedFeaturesBox <- renderUI({
        if (length(input$modelTable_rows_selected) == 0) {
            return()
        }
        selected <- selected_panel_features() %>%
            dplyr::group_by(cancer_id, featureset_id) %>%
            dplyr::summarize(feats = paste(feature_id, collapse = "<br/>")) %>%
            dplyr::mutate(
                html = sprintf(
                    "<b>%s %s</b><br/>%s", cancer_id, featureset_id, feats
                ),
            ) %>%
            dplyr::pull(html) %>%
            paste(collapse = "<br/><br/>")
        shiny::HTML(
            sprintf(
                "<div>%s</div>",
                selected
            )
        )
    })

    output$featureTypeBox <- renderPlotly({
        if (is.null(selected_panel_features())) {
            return()
        }
        selected_panel_features() %>%
            tidyr::separate(feature_id,
                            sep = "\\:",
                            into = c("datatype", "platform1", "platform2", "featureid1", "featureid2", "extra"),
                            remove = F) %>%
            dplyr::count(platform1) %>%
            dplyr::mutate(share = n / sum(n) * 100) %>%
            dplyr::arrange(desc(share)) %>%
            plot_ly(labels = ~platform1, values = ~share,  type = "pie",
                    textposition = 'inside',
                    textinfo = 'label+percent',
                    insidetextfont = list(color = '#FFFFFF', size = 15),
                    hoverinfo = 'text',
                    text = ~paste(round(share, digits = 2), '%'),
                    showlegend = FALSE)
    })

    output$featureFreqBox <- renderPlotly({
        if (is.null(selected_panel_features())) {
            return()
        }

        fcount <- selected_panel_features() %>%
            dplyr::group_by(feature_id) %>%
            dplyr::summarize(model_occurence = as.factor(dplyr::n())) %>%
            dplyr::ungroup() %>%
            dplyr::group_by(model_occurence) %>%
            dplyr::summarize(count = dplyr::n())

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

    output$performanceBox <- renderPlotly({
        if (length(input$modelTable_rows_selected) == 0) {
            return()
        }

        selected <- model_summary %>%
            dplyr::slice(input$modelTable_rows_selected) %>%
            dplyr::pull(prediction_id)

        sub_perf <- predictions %>%
            dplyr::filter(prediction_id %in% selected,
                          type == "testing") %>%
            dplyr::group_by(cancer_id, model_id, featureset_id, `repeat`, actual_value) %>%
            dplyr::summarize(correct = table(as.numeric(predicted_value) == as.numeric(actual_value))["TRUE"],
                             total = dplyr::n()) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(tpr = round(correct / total, digits = 3)) %>%
            dplyr::rename(Subtype = actual_value, TPR = tpr)

        g <- ggplot(sub_perf, aes(x = Subtype, y = TPR, fill = Subtype)) +
            geom_boxplot(alpha = 0.5) +
            theme_minimal(12) +
            labs(x = "", y = "") +
            theme(
                axis.text.x = element_text(angle = 45),
                legend.position = "none",
                panel.spacing = unit(2, "lines")
            ) +
            facet_wrap(~cancer_id, scales = "free_x", ncol = 3)
        ggplotly(g,  height = 250*ceiling(length(unique(sub_perf$cancer_id))/2)) %>%
            layout(margin = list(b = 50, t = 50, l = 10, r = 10))
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

        subtypes <- cancer_preds() %>%
            dplyr::select(sample_id, actual_value) %>%
            dplyr::distinct() %>%
            dplyr::arrange(base::match(sample_id, colnames(pred_mat))) %>%
            dplyr::pull(actual_value)

        tpr_model <- cancer_preds() %>%
            dplyr::filter(type == input$set_selection) %>%
            dplyr::group_by(prediction_id) %>%
            dplyr::summarize(correct = table(as.numeric(predicted_value) == as.numeric(actual_value))["TRUE"],
                             total = dplyr::n()) %>%
            dplyr::mutate(tpr = correct / total) %>%
            dplyr::arrange(base::match(prediction_id, rownames(pred_mat))) %>%
            dplyr::pull(tpr)

        tpr_sample <- cancer_preds() %>%
            dplyr::filter(type == input$set_selection) %>%
            dplyr::group_by(sample_id) %>%
            dplyr::summarize(correct = table(as.numeric(predicted_value) == as.numeric(actual_value))["TRUE"],
                             total = dplyr::n()) %>%
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
        hm %>% modify_layout(list(height = 400 + 15*dim(pred_mat)[1],
                                  autosize = TRUE))
    })

    output$featureFreq <- renderPlotly({
        fset <- featureSets %>% dplyr::filter(cancer_id  == input$cancer_selection)
        suppressMessages({
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
        })
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

    sample_sel <- debounce(reactive({input$sample_selection}), millis = 500)

    output$samplePredDetails <- renderPlotly({
        if (length(sample_sel()) > 0) {
            df_subset <- cancer_preds() %>%
                dplyr::filter(type == input$set_selection) %>%
                dplyr::filter(sample_id %in% sample_sel()) %>%
                dplyr::mutate(sample_label = paste(sample_id, actual_value, sep = " - Subtype: "))
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
                height = 400 + 10*length(unique(df_subset$prediction_id))*ceiling(length(sample_sel())/2)
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
    feat_sel <- debounce(reactive({input$feature_selection}), millis = 1000)

    # TODO change cancer filter to allow for multiple selections
    output$featureDetails <- renderPlotly({
        feats <- feat_sel()
        if (length(feats) > 0) {
            sub <- feature_con %>%
                dplyr::tbl(input$cancer_selection) %>%
                dplyr::filter(feature_id %in% feats) %>%
                dplyr::collect() %>%
                dplyr::rename(subtype_id = subtype)

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
