fluidPage(
  navbarPage(
  #  shinythemes::themeSelector(),
    theme=shinythemes::shinytheme("paper"), # To use a theme, uncomment this
    title="shinyBioTools",

    tabPanel(
      title="real-time PCR"
    ),


    # shRNA
    tabPanel(
      title="Easy shRNA",
      module_shRNA_ui("shRNA")
    ),
    # end of shRNA


    tabPanel(
      title="Easy sgRNA"
    )

  )
)
