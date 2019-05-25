## ui
module_shRNA_ui <- function(id) {
  ns <- NS(id)

fluidRow(
    column(2,
           wellPanel(
             radioButtons(ns("shrna.species"), "Choose species", c("hs", "mm"), "hs", inline=T),
             radioButtons(ns("shrna.type"), "Input Type", c("symbol", "ensembl_gene_id", "entrezgene"), "symbol"),
           #  checkboxInput(ns("shrna.alias"), "Alias convert", value=FALSE),
             numericInput(ns("shrna.number"), "shRNA number", value=3, min=1, max=6, step=1, width="150px"),
             textAreaInput(ns("shrna.input"), "Input Your Queries", height="120px", width="150px"),
             hr(),
             actionButton(ns("shrna.submit"), "Submit Gene"),
             br(),br(),
             actionButton(ns("shrna.design"), "Design Primer"),
             br(),br(),
             downloadButton(ns("shrna.dl_primer"), "Download")
           ) # end of wellpanel
    ),
    column(9, offset=1,
           tableOutput(ns("shrna.gene.convert")),
           br(),
           tableOutput(ns("shrna.antisense")),
           br(),
           tableOutput(ns('shrna.primer'))
           )
)

}
