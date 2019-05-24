
#'
#' @title id conversion
#'
#' @description convert given IDs (including alias) to specific IDs
#'
#' @param genelist a vector containing genes you want to convert from
#'
#' @param fromType *genelist* type, one of ensemble_gene_id (default), symbol, entrezgene
#'
#' @param species only recept hs (default), mm
#'
#' @param alias if *genelist* contains gene symbol alias (defalut is TRUE)
#'
#' @export
#'
id_convert <- function(genelist=NULL,
                       fromType="ensembl_gene_id",
                       species="hs",
                       alias=T) {

  # check genelist
  if(!is.vector(genelist))
    stop("genelist is not a vector")
  genelist <- tibble::enframe(genelist, name=NULL, value="query")

  # check fromType
  if(!(fromType %in% c("ensembl_gene_id", "symbol", "entrezgene")))
    stop("fromType is not one of ensembl_gene_id, symbol, entrezgene")

  print(getwd())
  # check species parameter
  if(species %in% c("hs", "mm")) {
    if(species=="hs") {
      # import annotation data
      ncbi_gene <- system.file("data", "Homo_sapiens.gene_info.gz", "shinyBioTools")
      anno <- AnnotationHub::GRCh38_annotation
    } else {
      ncbi_gene <- system.file("data", "Mus_musculus.gene_info.gz", "shinyBioTools")
      anno <- AnnotationHub::GRCm38_annotation
    }

  } else {
    stop("species must be one of hs, mm")
  }

  # alias to symbol
  if(fromType=="symbol" & isTRUE(alias)) {
    if(species=="hs") {
      genelist$toSymbol <- stringr::str_to_upper(genelist$query)
    } else{
      genelist$toSymbol <- stringr::str_to_title(genelist$query)
    }
    genelist$symbol <- limma::alias2SymbolUsingNCBI(genelist$toSymbol,
                                             gene.info.file=ncbi_gene,
                                             required.columns="Symbol") %>%
      dplyr::pull()
  #  print(genelist)

    # find convert missing genes
    genelist.miss <- dplyr::filter(genelist, is.na(symbol)) %>% dplyr::pull(query)

    if(length(genelist.miss) > 0)
      warning(paste0(paste(genelist.miss, collapse=", "), " : were not converted correctly with alias2Symbol"))

    # convert IDs
    re <- dplyr::filter(anno, !!as.name(fromType) %in% na.omit(genelist$symbol)) %>%
      dplyr::left_join(genelist, ., by=c("symbol"="symbol")) %>%
      dplyr::select(-toSymbol)

  } else {
    re <- dplyr::filter(anno, !!as.name(fromType) %in% na.omit(genelist$query)) %>%
      dplyr::left_join(genelist, ., by=c("query"=fromType))
  }

  return(re)
}
