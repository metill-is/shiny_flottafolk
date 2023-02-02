ui <- shinyUI(
    navbarPage(
        title = "Flóttafólk og hælisleitendur",
        theme = bs_global_get(),
        
        
        make_module_ui(
            dat = d,
            group_var_label = "Valin lönd verða í forgrunni ásamt Íslandi",
            group_var_selected = c("Noregur", "Danmörk", "Svíþjóð", "Finnland"),
            variable_label = "Veldu breytu",
            sidebar_info = sidebar_info,
            plot_height = "600px"
        )("module")
        
    )
)