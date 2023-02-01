make_module_ui <- function(
        dat,
        group_var_label = "group",
        group_var_selectize = TRUE,
        group_var_selected = unique(data$group)[1],
        variable_label = "variable",
        sidebar_info = NULL
) {
    module_ui <- function(
        id,
        data = dat
    ) {
        sidebarLayout(
            sidebarPanel(
                selectInput(
                    inputId = NS(id, "group"),
                    label = group_var_label,
                    choices = unique(data$group),
                    selectize = group_var_selectize,
                    selected = group_var_selected,
                    multiple = TRUE
                ),
                selectInput(
                    inputId = NS(id, "variable"),
                    label = variable_label,
                    choices = unique(data$variable),
                    selectize = FALSE,
                    multiple = FALSE
                ),
                div(
                    actionButton(
                        inputId = NS(id, "goButton"),
                        label = "Sækja gögn",
                        width = "120px"
                    ),
                    class = "center", align = "middle"
                ),
                HTML(sidebar_info)
            ),
            
            mainPanel(
                plotlyOutput(NS(id, "line_plot"))
            )
        )
    }
    
    module_ui
}

make_module_server <- function(
        dat
) {
    module_server <- function(id, data = dat) {
        moduleServer(id, function(input, output, session) {
            output$line_plot <- renderPlotly({
                p <- data |>
                    filter(
                        group %in% input$group,
                        variable %in% input$variable
                    ) |>
                    ggplot(aes(time, value)) +
                    geom_line(aes(group = group, colour = group)) +
                    geom_point(aes(group = group, colour = group)) +
                    scale_x_date(
                        breaks = breaks_width("month"),
                        labels = label_date_short()
                    ) +
                    theme(
                        legend.position = "bottom",
                        plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
                    ) +
                    labs(
                        colour = NULL,
                        x = NULL,
                        y = NULL
                    )
                
                metill_ggplotly(p) |> 
                    layout(
                        legend = list(
                            orientation = "h",
                            valign = "bottom",
                            y = -0.2
                        )
                    )
            }) |>
                bindEvent(
                    input$goButton,
                    ignoreNULL = FALSE
                )
        })
    }
    
    module_server
}


