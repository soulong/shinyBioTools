
#'
#' @title search splashRNA for a single query
#'
#' @description search splashRNA for a single query
#'
#' @param id entrezgene id
#'
#' @param n number of returned shRNA
#'
#' @param anno names for each shRNA
#'
splashRNA <- function(id, n, anno) {
  entrezid <- stringr::str_c("> entrezID","\n",id)
  res <- httr::POST(url = "http://splashrna.mskcc.org/show_results",
              body = list(fasta=entrezid,
                          n_predictions=as.character(n),
                          removeRE="on",
                          apa="on",
                          academic="on",
                          email="haohe@sioc.ac.cn"),
              httr::add_headers(Origin="http://splashrna.mskcc.org",
                                Referer="http://splashrna.mskcc.org/"),
              # User-Agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36"),
              encode = "form")
  seq <- c()
  score <- c()
  seq_name <- c()
  i <- 1
  while(i < n+1)
  {
    nodepath_seq <- stringr::str_c("/html/body/div[2]/div[2]/div/table/tbody/tr[", i, "]/td[2]")
    nodepath_score <- stringr::str_c("/html/body/div[2]/div[2]/div/table/tbody/tr[", i, "]/td[3]")
    seq_new <- xml2::read_html(res) %>%
      rvest::html_node(xpath=nodepath_seq) %>%
      rvest::html_text(trim=TRUE)
    score_new <- xml2::read_html(res) %>%
      rvest::html_node(xpath=nodepath_score) %>%
      rvest::html_text(trim=TRUE)
    seq <- c(seq, seq_new)
    score <- c(score, score_new)
    seq_name <- c(seq_name, stringr::str_c(anno,"#",i))
    i <- i+1
    Sys.sleep(2)
  }
  return(data.frame(ID=seq_name, Antisense=seq, Score=score, stringsAsFactors=FALSE))
}




#'
#' @title search splashRNA for batch query
#'
#' @description search splashRNA for batch query
#'
#' @param ID entrezgene ids
#'
#' @param N number of returned shRNA
#'
#' @param Anno names for each shRNA
#'
#' @export
#'
splashRNA_batch <- function(ID, N, Anno)
{
  dt_merge <- data.frame()
  for(i in 1:length(ID))
  {
    #    ID <- as.character(ID)
    dt <- splashRNA(ID[i], N, Anno[i])
    dt_merge <- rbind(dt_merge, dt)
    dt_merge <- na.omit(dt_merge)
  }
  return(dt_merge)
}

# write.csv(dt_merge, "SplashRNA_Result.csv", row.names=FALSE)
