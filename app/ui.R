ui <- shinyUI(
    navbarPage(
        "Flóttafólk",
        theme = bs_global_get(),
        
        
        make_module_ui(
            dat = d,
            group_var_label = "Land",
            group_var_selected = c("Ísland", "Noregur", "Danmörk"),
            variable_label = "Veldu breytu",
            sidebar_info = sidebar_info
        )("module")
        
    )
)