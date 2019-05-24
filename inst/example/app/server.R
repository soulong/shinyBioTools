
server <- function(input, output, session) {

  callModule(module_shRNA, "shRNA", shRNA_vector)

}
