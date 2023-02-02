make_module_ui <- function(
        dat,
        group_var_label = "group",
        group_var_selectize = TRUE,
        group_var_selected = unique(data$group)[1],
        variable_label = "variable",
        sidebar_info = NULL,
        plot_title = NULL,
        plot_subtitle = NULL,
        plot_height = "800px"
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
                div(
                    h4(textOutput(NS(id, "plot_title"))),
                    class = "center",
                    align = "middle"
                ),
                div(
                    p(textOutput(NS(id, "plot_subtitle"))),
                    class = "center",
                    align = "middle"
                ),
                plotlyOutput(NS(id, "line_plot"), height = plot_height)
            )
        )
    }
    
    module_ui
}

make_module_server <- function(
        dat,
        time_name = "Dagsetning",
        plot_title = " ",
        plot_subtitle =  ""
) {
    module_server <- function(id, data = dat) {
        moduleServer(id, function(input, output, session) {
            
            output$plot_title <- renderText({
                glue(plot_title)
            }) |> 
                bindEvent(input$goButton,
                          ignoreNULL = FALSE)
            
            output$plot_subtitle <- renderText({
                glue(plot_subtitle)
            }) |> 
                bindEvent(
                    input$goButton,
                    ignoreNULL = FALSE
                )
            
            output$line_plot <- renderPlotly({
                
                colour_table <- tibble(
                    group = c(input$group, "Ísland"),
                    colour = c(brewer.pal(n = length(input$group), name = "Dark2"), iceland_col)
                ) |> 
                    mutate(
                        group = fct_reorder(group, make_clean_names(group))
                    ) |> 
                    arrange(group)
                
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
                            group %in% input$group ~ 0.7,
                            TRUE ~ 0.1
                        ),
                        linewidth = case_when(
                            group == "Ísland" ~ 2,
                            group %in% input$group ~ 1,
                            TRUE ~ 0.2
                        ),
                        text = glue(str_c(
                            "<b align='center'>{group}</b>", "\n",
                            "{time_name}: {time}", "\n",
                            "{input$variable}: {number(value)}"
                        ))
                    ) |> 
                    arrange(group)
                
                y_max <- plot_dat |> 
                    filter(
                        group %in% c(input$group, "Ísland")
                    ) |> 
                    pull(value) |> 
                    max(na.rm = T)
                
                p <- plot_dat |>
                    ggplot(aes(time, value, colour = group, group = group, alpha = alpha, text = text)) +
                    geom_line(
                        data = plot_dat |> 
                            filter(!group %in% c("Ísland", input$group)),
                        linewidth = 0.3,
                        colour = "black"
                    ) +
                    geom_line(
                        data = plot_dat |> 
                            filter(group %in% c(input$group)),
                        # aes(colour = group),
                        linewidth = 0.6
                    ) +
                    geom_line(
                        data = plot_dat |> 
                            filter(group == "Ísland"),
                        # aes(colour = group),
                        linewidth = 1
                    ) +
                    geom_point(
                        data = plot_dat |> 
                            filter(group == "Ísland"),
                        # aes(colour = group),
                        size = 3
                    ) +
                    scale_x_date(
                        breaks = breaks_width("month"),
                        labels = label_date_short()
                    ) +
                    scale_colour_manual(
                        values = colour_table |> 
                            # filter(group %in% c("Ísland", input$group)) |> 
                            # distinct(group, colour) |>
                            # arrange(desc(group)) |> 
                            pull(colour)
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
                        y = NULL
                    )
                
                ggplotly(
                    p,
                    tooltip = "text"
                ) |> 
                    layout(
                        hoverlabel = list(align = "left"),
                        margin = list(t = 0)
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


