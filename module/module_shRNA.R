## server
module_shRNA <- function(input, output, session, shRNA_vector) {

  # get gene info
  shrna.geneinfo <- eventReactive(input$shrna.submit, {
    shrna.ids <- as.character(stringr::str_split(input$shrna.input, "\n", simplify=T))
    shrna.gene <- shinyBioTools::id_convert(shrna.ids, input$shrna.type,
                                            input$shrna.species) %>%   # discard input$shrna.alias
      dplyr::select(1:6)
    return(shrna.gene)
  })

  # display converted gene
  output$shrna.gene.convert <- renderTable({
    shrna.geneinfo()
  })

  # find shRNA and score according to splashRNA site
  splashRNA <-  eventReactive(input$shrna.design,{
    shrna.gene.entrezid <- dplyr::filter(shrna.geneinfo(), !is.na(entrezgene))
    antisense <- shinyBioTools::splashRNA_batch(
      unique(dplyr::pull(shrna.gene.entrezid, entrezgene)),
      input$shrna.number,
      shrna.gene.entrezid$symbol)
    primer <- shinyBioTools::shRNA_primer(antisense, shRNA_vector)
    return(list(antisense=antisense, primer=primer))
  })


  # print designed shRNA antisense
  # output$out_id <- renderText({input$entrezid})
  output$shrna.antisense <- renderTable({
    if (is.null(splashRNA())) return()
    splashRNA()$antisense
  })

  ## print designed primer and construct
  output$shrna.primer <- renderTable({
    if (is.null(splashRNA())) return()
    splashRNA()$primer
  })

  ## handle download
  output$shrna.dl_primer <- downloadHandler(
    filename=paste0(as.character(Sys.Date()), "_shRNA_Primers.xlsx"),
    content=function(path) writexl::write_xlsx(list(antisense=splashRNA()$antisense, primer=splashRNA()$primer), path),
    contentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
}


