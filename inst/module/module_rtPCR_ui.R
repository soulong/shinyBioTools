
module_rtPCR_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(width=2, # left side bar column
           wellPanel(
             selectInput(ns("equip"),"Choose equipment", c("Quantstudio_5","Quantstudio_6"), "Quantstudio_5"),
             fileInput(ns("userfile"), 'Upload File', accept=c(".xls")),
             selectInput(ns("target"), "Choose targets", choices=NULL, multiple = TRUE),
             selectInput(ns("sample"), "Choose samples", choices=NULL, multiple = TRUE),
             checkboxInput(ns("na.do"), "Auto deal with NA values", value=TRUE),
             hr(),
             actionButton(ns("apply"), 'Submmit'),
             br(),br(),
             downloadButton(ns("download_plot"), 'Download')
           )
    ), # end od left side bar column

    # main plot area
    column(width=10,
           tabsetPanel(
             tabPanel('Facet Plot',
                      fluidRow(
                        column(10,
                               uiOutput(ns("plot_facet_ui"))
                               ),
                        column(2, align='right',
                               sliderInput(ns('plotwidth_facet'), 'Plot width', min=100, max=1000, value=700, step=20),
                               sliderInput(ns('plotheight_facet'), 'Plot height', min=100, max=1000, value=500, step=20),
                               sliderInput(ns('size_facet'), 'X-axis lab size', min=1, max=50, value=15, step=1),
                               sliderInput(ns('xangle_facet'), 'X-axis lab angle', min=0, max=90, value=45, step=5)                        ))),
             tabPanel('Combine Plot',
                      fluidRow(
                        column(10, uiOutput(ns("plot_combine_ui")), align='center'),
                        column(2,  align='right',
                               sliderInput(ns('plotwidth_combine'), label='Plot width', min=100, max=1000, value=700, step=20),
                               sliderInput(ns('plotheight_combine'), label='Plot height', min=100, max=1000, value=500, step=20),
                               sliderInput(ns('size_combine'), label='X title size', min=1, max=50, value=15, step=1),
                               sliderInput(ns('xangle_combine'), label='X title angle', min=0, max=90, value=45, step=5),
                               sliderInput(ns('legendsize_combine'), label='Legend size', min=1, max=50, value=15, step=1)
                               #            sliderInput(ns('pian', label='Legend size', min=0, max=1, value=0.2, step=0.05)
                        ))),
             tabPanel('Data Summary',
                      tableOutput(ns("table_summary")),
                      hr(),
                      h5('These samples hava NA value, which may affect the result:'),
                      verbatimTextOutput('na_sample'),
                      h5('These targets hava NA value, which may affect the result:'),
                      verbatimTextOutput('na_target')),
             tabPanel('Melting Curve',
                      plotOutput(ns("plot_mc_1"), width='1000px'),
                      hr(),
                      plotOutput(ns("plot_mc_2"), width='800px')
             )
           ) # end of tabset
    ) # end of column
  )

}
