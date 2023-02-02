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
                    choices = unique(data |> filter(group != "Ísland") |> pull(group)),
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
        dat,
        plot_title = NULL,
        plot_subtitle = NULL
) {
    module_server <- function(id, data = dat) {
        moduleServer(id, function(input, output, session) {
            
            output$line_plot <- renderPlotly({
                
                colour_table <- tibble(
                    group = union("Ísland", input$group),
                    colour = c(iceland_col, brewer.pal(n = length(input$group), name = "Dark2"))
                ) |> 
                    arrange(group == "Ísland")
                
                plot_dat <- d |>
                    filter(
                        variable %in% input$variable
                    ) |> 
                    left_join(
                        colour_table,
                        by = "group"
                    ) |> 
                    mutate(
                        colour = coalesce(colour, "grey70"),
                        alpha = case_when(
                            group == "Ísland" ~ 1,
                            group %in% input$group ~ 0.5,
                            TRUE ~ 0.1
                        ),
                        linewidth = case_when(
                            group == "Ísland" ~ 2,
                            group %in% input$group ~ 1,
                            TRUE ~ 0.2
                        )
                    ) |> 
                    arrange(group)
                
                y_max <- plot_dat |> 
                    filter(
                        group %in% c(input$group, "Ísland")
                    ) |> 
                    pull(value) |> 
                    max(na.rm = T)
                
                p <- plot_dat |>
                    ggplot(aes(time, value, group = group, alpha = alpha)) +
                    geom_line(
                        data = plot_dat |> 
                            filter(!group %in% c("Ísland", input$group)),
                        linewidth = 0.3
                    ) +
                    geom_line(
                        data = plot_dat |> 
                            filter(group %in% c(input$group)),
                        aes(colour = group),
                        linewidth = 0.6
                    ) +
                    geom_line(
                        data = plot_dat |> 
                            filter(group == "Ísland"),
                        aes(colour = group),
                        linewidth = 1
                    ) +
                    geom_point(
                        data = plot_dat |> 
                            filter(group == "Ísland"),
                        aes(colour = group),
                        size = 3
                    ) +
                    scale_x_date(
                        breaks = breaks_width("month"),
                        labels = label_date_short()
                    ) +
                    scale_colour_manual(
                        values = colour_table |> distinct(group, colour) |> pull(colour)
                    ) +
                    scale_size_identity() +
                    scale_alpha_identity() +
                    coord_cartesian(ylim = c(0, y_max * 1.05)) +
                    theme(
                        legend.position = "right",
                        plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
                    ) +
                    labs(
                        colour = NULL,
                        x = NULL,
                        y = NULL,
                        title = glue(plot_title),
                        subtitle = glue(plot_subtitle)
                    )
                
                ggplotly(
                    p,
                    height = 500
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


