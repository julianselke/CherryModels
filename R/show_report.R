#' Display a report in a shiny app
#'
#' Click on the `i` button in the top right corner for help.
#'
#' @param data data.frame returned by `cherry_models()` or `get_process_results()`
#'
#' @return starts a shiny app
#' @import shiny
#' @import psych
#' @import ggplot2
#' @import DT
#' @import shinyBS
#' @importFrom rje cubeHelix
#' @export
#'
#' @examples
#'
#' x <- cherry_models(
#'   model_data = mpg,
#'   response = "cty",
#'   fixed_inter = c("displ", "cyl", "year"),
#'   fixed_no_inter = "hwy",
#'   max_comb = 3,
#'   comb_comb = 2,
#'   random = "(1|model)",
#'   n.workers = 2,
#'   dry = FALSE,
#'   background = FALSE
#' )
#'
#' show_report(x)
#'
show_report <- function(data) {
  runApp(list(

    ui =
      fluidPage(
        style = "padding: 0px;",
        actionButton(inputId = "info_button",
                     label = "",
                     icon = icon("info"),
                     style = "position: absolute; top: 10px; right: 60px; z-index:10000;"),
        actionButton(inputId = "gear_button",
                     label = "",
                     icon = icon("gear"),
                     style = "position: absolute; top: 10px; right: 10px; z-index:10000;"),
        bsModal(id = "info_modal",
                title = "",
                trigger = "info_button",
                size = "medium",
                htmlOutput("tab_info")),
        bsModal(id = "plot_height_modal",
                title = "Plot Height",
                trigger = "gear_button",
                size = "medium",
                sliderInput(inputId = "plot_height",
                            width = "100%",
                            label = "",
                            min = 300,
                            max = 3000,
                            value = 750,
                            step = 50)),

        navbarPage(
          id = "tab",
          span(HTML("&#127826;CherryModels")),

          tabPanel(title = "Summary",
                   width = "100%",
                   htmlOutput("summary_tab")),

          tabPanel("Correlations",
                   div(plotOutput("correlations_tab", width = "95%"), align = "center")
          ),

          tabPanel(title = "Models",
                   width = "100%",
                   DT::DTOutput("models_tab")),


          tabPanel(title = "Frequencies",
                   tabsetPanel(
                     tabPanel(title = "Initial Terms",
                              width = "100%",
                              tags$div(
                                style = "margin-top:5px;",
                                DT::DTOutput("initial_terms_tab")
                              )
                     ),
                     tabPanel(title = "Retained Terms",
                              width = "100%",
                              tags$div(style = "margin-top:5px;",
                                       DT::DTOutput("retained_terms_tab")
                              )
                     ),
                     tabPanel(title = "Fixed Effects",
                              div(plotOutput("fixed_effects_tab", width = "95%"),
                                  align = "center")
                     ),
                     tabPanel(title = "Interaction Terms",
                              div(plotOutput("interaction_terms_tab", width = "95%"),
                                  align = "center")
                     )
                   )
          ),

          tabPanel(title = "AICs",
                   tabsetPanel(
                     tabPanel(title = "Initial vs. Final AIC",
                              div(plotOutput("aic_plot_tab", width = "95%"),
                                  align = "center")
                     ),
                     tabPanel(title = "Linear Model",
                              div(plotOutput("linear_model_tab", width = "85%"),
                                  align = "center")
                     ),
                     tabPanel(title = "Linear Model Result",
                              div(plotOutput("linear_model_result_tab", width = "95%"),
                                  align = "center")
                     )
                   )
          )
        )
      ),

      server = function(input, output, session) {

        output$summary_tab <- renderUI({
          HTML(
            paste(c(
              "<center><hr><h3><b>Summary of",
              format(nrow(data), big.mark = ",", scientific = FALSE),
              "computed models.</b></h3>
        <hr><br><br><br>
        <h3><b>Best Model(s):</b></h3>
        <br><b>",
              paste(unique(data$Model.final[data$AIC.final == min(data$AIC.final)]),
                    collapse = "<br><br>"),
              "</b><br><br><br><br><br><hr><br>
        <h3><b>Tested Terms:</b></h3>
        <br>
        <h5><b>Response:</b><br><br>",
              attr(data, "response"),
              "</h5><br>
        <h5><b>Fixed Effects with Interaction:</b>
        <br><br>",
              paste(c(attr(data, "fixed_inter")), collapse = "<br><br>"),
              "</h5><br>
        <h5><b>Fixed Effects WITHOUT Interaction:</b>
        <br><br>",
              paste(c(attr(data, "fixed_no_inter")), collapse = "<br><br>"),
              "</h5><br>
        <h5><b>Random Effects:</b>
        <br><br>",
              paste(c(attr(data, "random")), collapse = "<br><br>"),
              "</h5><br><br><br><br><br><br><br><br><br><hr>
        <p>made with &#128153; in <b>R</b><p></center>")
            )
          )
        })

        output$tab_info <- renderText({

          summary_tab_info <- HTML(
            "I'm an info button and I will help you to make sense of this report.
              The content of this starting page should be self explaining but this may not
              be true for all tabs.
              Just click me and I will display helpful messages.
            <br>
            The button to the right of me (showing a gear icon) opens a window with a slider to
            adjust plot sizes, since the plot widths are fixed but adequat plot height depends on
            the data."
          )

          correlations_tab_info <- HTML(
            "In this figure you see all pairwise correlations of the fixed effects in the
          model data. The lower triangle shows bivariate scatterplots including correlation
          ellipses. On the diagonal histograms and density curves of the stated variable are
          displayed and the upper triangle displays the Pearson correlation coefficient.
          You can find details in `?psych::pairs.panels`"
          )

          models_tab_info <- HTML(
            "Here's a table of all models that were calculated. From left to right the columns
          display the:
          <br><br>
          - formula of the initial model, i.e. before passing it to `lmer::step()`
          <br><br>
          - AIC of the initial model
            <br><br>
          - formula of the final model, i.e. after reduction
            <br><br>
          - AIC of the final model
            <br><br>
          - fraction of terms that were dropped during model reduction
            <br><br>
            - the fixed effects retained in the final model"
          )


          frequencies_tab_info <- HTML(
            "The frequencies of terms/effects are split among four sub-tabs:
            <hr>
            <b>Initial Terms</b>: The frequencies of terms in the initial formulas.
            <hr>
            <b>Retained Terms</b>: The frequencies of terms in the final formulas.
            <hr>
            <b>Fixed Effects</b>: A barplot showing the frequency (count) of singular effects
            in the final models and the mean AIC of the models the effects occurred in.
            The error bars extend to the AIC value of the models with the lowest and highest AIC,
            respectively.
            <hr>
            <b>Interaction Terms</b>: A barplot showing the frequency (count) of interaction terms
            in the final models and the mean AIC of the models the terms occurred in.
            The error bars extend to the AIC value of the models with the lowest and highest AIC,
            respectively.
            <hr>
            Effects/terms occurring exclusively in models with relatively low AIC point to an
            increased importance of these effects/terms in explaining your data.
            ")

          aics_tab_info <- HTML(
            "The three sub-tabs display further information on the AIC values. The first tab
            shows a scatterplot of initial vs. final AICs. Coloring indicates the number of
            effects that were dropped and the diagonal line indicates `y=x`.
            <br>
            <br>
            To further assess the importance of the retained terms a linear model was fitted.
            The dependent variable is the AIC and the count of retained terms is the independent
            variable.
            <br>
            <br>
            The second sub-tab (<b>Linear Model</b>) shows the familiar model plots and the
            third sub-tab (<b>Linear Model Result</b>) displays the estimate of respective terms,
            i.e. the change in AIC associated with the term on the x-axis. The coloring indicates
            the p-value on a decimal logarithmic scale. p-values > 0.05 are colored in gray.
            <br>
            <br>
            A negative estimate indicates that models including this term had a lower AIC, i.e.
            performed better in eexplaining your data.
            ")

          case_when(input$tab == "Summary" ~ summary_tab_info,
                    input$tab == "Models" ~ models_tab_info,
                    input$tab == "Correlations" ~ correlations_tab_info,
                    input$tab == "Frequencies" ~ frequencies_tab_info,
                    input$tab == "AICs" ~ aics_tab_info
                    )

          })

        output$models_tab <- DT::renderDT({
          data %>%
            mutate(across(matches("AIC"), ~round(.x, 1))) %>%
            mutate(Model.init = gsub("(~|\\+|\\*)", "\\1<br/>", Model.init)) %>%
            mutate(across(is.character, ~ as.factor(.x))) %>%
            rename(Fixed.retained = Fixed) %>%
            select(3,1,4,2,5,6) %>%
            DT::datatable(rownames = FALSE, filter = "top", escape = FALSE)
        })

        output$correlations_tab <- renderPlot({
          suppressWarnings(
          attr(data, "model_data") %>%
            select(c(attr(data, "fixed_inter"), attr(data, "fixed_no_inter"))) %>%
            psych::pairs.panels(ci = TRUE, stars = TRUE)
          )

        }, height = function() {
          req(input$plot_height)
          })

        output$retained_terms_tab <- renderDT({
          dat <- data %>%
            select(Fixed.retained = Fixed) %>%
            table() %>%
            as.data.frame() %>%
            arrange(desc(Freq))

          dat %>%
            DT::datatable() %>%
            formatStyle("Freq",
                        background = styleColorBar(range(dat$Freq), 'lightblue'),
                        backgroundSize = '98% 88%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center')
        })

        output$initial_terms_tab <- renderDT({
          dat <- data %>%
            select(Model.init) %>%
            mutate(Model.init = gsub("^.+\\~", "", Model.init)) %>%
            mutate(Model.init = gsub("\\+ \\(.$", "", Model.init)) %>%
            separate_longer_delim(Model.init, delim = regex("\\+")) %>%
            mutate(Model.init = str_trim(Model.init)) %>%
            table() %>%
            as.data.frame() %>%
            arrange(desc(Freq))

          dat %>%
            DT::datatable() %>%
            formatStyle("Freq",
                        background = styleColorBar(range(dat$Freq), 'lightblue'),
                        backgroundSize = '98% 88%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center')
        })

        output$aic_plot_tab <- renderPlot({
          data %>% separate(Reduced, into = c("denR", "enuR"), sep = " / ") %>%
            mutate(denR = as.numeric(denR)) %>%
            ggplot() +
            aes(x = AIC.init, y = AIC.final, fill = denR) +
            geom_abline(intercept = 0, slope = 1) +
            geom_point(shape = 21, size = 2, alpha = 0.7) +
            scale_fill_gradientn(colours = rev(
              rje::cubeHelix(15, start = 0.5, r = -1.5, hue = 1, gamma = 1)),
              name = "No. of dropped effects\n") +
            labs(x = "\nAIC.init", y = "AIC.final\n") +
            theme_bw() +
            coord_equal() +
            shiny_plot_theme() +
            theme(legend.position = "top",
                  legend.key.width = unit(3, "cm"))
        }, height = function() {
          req(input$plot_height)
          })

        output$fixed_effects_tab <- renderPlot({
          data %>%
            select(AIC.init, AIC.final, Fixed) %>%
            separate_longer_delim(Fixed, delim = regex("[:+]")) %>%
            mutate(Fixed = as.factor(str_trim(Fixed))) %>%
            group_by(Fixed) %>%
            summarize(Mean.AIC = mean(AIC.final),
                      Min.AIC = min(AIC.final),
                      Max.AIC = max(AIC.final),
                      Count = n()) %>%
            {ggplot(.) +
                aes(x = fct_reorder(Fixed, Mean.AIC, .desc = TRUE),
                    y = Mean.AIC, fill = Count, group = Fixed) +
                geom_col(position = position_dodge2(), color = "gray20", linewidth = 0.1) +
                geom_errorbar(aes(ymin = Min.AIC, ymax = Max.AIC),
                              linewidth = 2,
                              color = "#ffffff",
                              width = 0.68) +
                geom_errorbar(aes(ymin = Min.AIC, ymax = Max.AIC),
                              linewidth = 1.5,
                              color = "gray20",
                              width = 0.66) +
                scale_fill_gradientn(colours = rev(
                  rje::cubeHelix(15, start = 0.5, r = -1.5, hue = 1, gamma = 1)),
                  name = "Count\n") +
                theme_classic() +
                shiny_plot_theme() +
                theme(legend.position = "top",
                      legend.key.width = unit(3, "cm")) +
                labs(y = "\nMean AIC\n\n\n", x = "", title = "") +
                ylim(0, max(.$Mean.AIC) * 1.1) +
                coord_flip(expand = FALSE)}
        }, height = function() {
          req(input$plot_height)
        })

        output$interaction_terms_tab <- renderPlot({
          data %>%
            select(AIC.init, AIC.final, Fixed) %>%
            separate_longer_delim(Fixed, delim = regex("[+]")) %>%
            mutate(Fixed = as.factor(str_trim(Fixed))) %>%
            group_by(Fixed) %>%
            summarize(Mean.AIC = mean(AIC.final),
                      Min.AIC = min(AIC.final),
                      Max.AIC = max(AIC.final),
                      Count = n()) %>%
            {ggplot(.) +
                aes(x = fct_reorder(Fixed, Mean.AIC, .desc = TRUE),
                    y = Mean.AIC, fill = Count, group = Fixed) +
                geom_col(position = position_dodge2(), color = "gray20", linewidth = 0.1) +
                geom_errorbar(aes(ymin = Min.AIC, ymax = Max.AIC),
                              linewidth = 2,
                              color = "#ffffff",
                              width = 0.68) +
                geom_errorbar(aes(ymin = Min.AIC, ymax = Max.AIC),
                              linewidth = 1.5,
                              color = "gray20",
                              width = 0.66) +
                ylim(0, max(.$Mean.AIC) * 1.1) +
                scale_fill_gradientn(colours = rev(
                  rje::cubeHelix(15, start = 0.5, r = -1.5, hue = 1, gamma = 1)),
                  name = "Count\n") +
                theme_classic() +
                shiny_plot_theme() +
                theme(legend.position = "top",
                      legend.key.width = unit(3, "cm")
                ) +
                labs(y = "\nMean AIC\n\n\n", x = "", title = "") +
                coord_flip(expand = FALSE)}
        }, height = function() {
          req(input$plot_height)
        })

        output$linear_model_tab <- renderPlot({
          aiclm <- data %>%
            select(AIC.init, AIC.final, Fixed) %>%
            separate_longer_delim(Fixed, delim = regex("[+]")) %>%
            mutate(Fixed = as.factor(str_trim(Fixed))) %>%
            select(Fixed, AIC.init) %$%
            lm(AIC.init ~ Fixed)

          plot_lm <- function(lm){
            par(mfrow = c(2, 2), mai = c(0.4, 0.8, 0.4, 0.2))
            plot(lm, ask = FALSE, which = 1)
            plot(lm, ask = FALSE, which = 2)
            plot(lm, ask = FALSE, which = 3)
            plot(lm, ask = FALSE, which = 5)
            par(mfrow=c(1, 1), mai = rep(1, 4))
          }

          plot_lm(aiclm)
        }, height = function() {
          req(input$plot_height)
        })

        output$linear_model_result_tab <- renderPlot({
          data %>%
            select(AIC.init, AIC.final, Fixed) %>%
            separate_longer_delim(Fixed, delim = regex("[+]")) %>%
            mutate(Fixed = as.factor(str_trim(Fixed))) %>%
            select(Fixed, AIC.init) %$%
            lm(AIC.init ~ Fixed) %>%
            summary() %>%
            `$`("coefficients") %>%
            as.data.frame() %>%
            select(Estimate, p = `Pr(>|t|)`) %>%
            mutate(pp = log10(ifelse(signif(p < 0.05), p, NA))) %>%
            rownames_to_column("var") %>%
            mutate(var = gsub("Fixed", "", var)) %>%
            filter(var != "(Intercept)") %>%
            ggplot() +
            aes(y = fct_reorder(var, Estimate, .desc = TRUE),
                x = Estimate,
                fill = pp) +
            geom_col(color = "gray20") +
            geom_col(data = . %>% filter(pp == -Inf), fill = "#000000") +
            xlab("\nEstimate\n\n\n") +
            scale_fill_gradientn(colours =
              rje::cubeHelix(15, start = 0.5, r = -1.5, hue = 1, gamma = 1),
              na.value = "gray90", name = "log10(P)\n"
            ) +
            scale_y_discrete(position = "right", name = "")  +
            theme_classic() +
            theme(legend.position = "top",
                  legend.key.width = unit(2, "cm")) +
            shiny_plot_theme() +
            geom_vline(xintercept = 0, linetype = "dashed")
        }, height = function() {
          req(input$plot_height)
        })
      }
      ))
}

shiny_plot_theme <- function() {
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold")
  )
}
