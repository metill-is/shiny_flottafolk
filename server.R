function(input, output, session) {
    
    make_module_server(
        dat = d,
        plot_title = "{input$variable}",
        plot_subtitle = "Sýnt sem fjöldi á 1.000 íbúa hvers lands"
    )("module")
    
}