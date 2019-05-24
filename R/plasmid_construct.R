

# loop <- DNAString("TAGTGAAGCCACAGATGTA")
# overlap_5 <- DNAString("TGCTGTTGACAGTGAGCG")
# overlap_3 <- DNAString("TGCCTACTGCCTCGGACT")
# control <- readDNAStringSet("D:/Scripts/R/Shiny_Bio-tools/www/pGIPZ-mirE-control.fa", "fasta")[[1]]

# input_file <- read.csv("SplashRNA_Result.csv", header=TRUE, stringsAsFactors=FALSE)[,1:2]
# colnames(input_file) <- c("ID", "Sequence")
# multi_antisense_guide <- input_file$Sequence

#'
#' @title construct plasmid
#'
#' @description construct a DNA fragment to a vector
#'
#' @param splashRNA splashRNA_batch result
#'
#' @param vector vector fasta
#'
#' @export
#'
primer_design <- function(splashRNA, control_fa)
{
  loop <- DNAString("TAGTGAAGCCACAGATGTA")
  overlap_5 <- DNAString("TGCTGTTGACAGTGAGCG")
  overlap_3 <- DNAString("TGCCTACTGCCTCGGACT")
  multi_antisense_guide <- splashRNA$Antisense
  Primer <- c()
  Sequence <- c()
  for(i in 1:length(multi_antisense_guide))
  {
    antisense_guide <- DNAString(multi_antisense_guide[[i]])
    mismatch_base <- DNAString(switch(as.character(antisense_guide[22]),
                                      "A"="C","G"="A","C"="A","T"="C"))
    sense_guide <- append(mismatch_base, reverseComplement(antisense_guide[-22]))
    insert_fragment <- c(sense_guide, loop, antisense_guide)
    new_fullseq <- replaceLetterAt(control_fa, 5424:5486, insert_fragment)
    primer_F <- as.character(str_c(loop, antisense_guide, overlap_3))
    primer_R <- as.character(reverseComplement(DNAString(str_c(overlap_5, sense_guide, loop))))
    id <- splashRNA$ID[i]
    name_fa <- str_c(id, ".fa")
    writeXStringSet(DNAStringSet(new_fullseq),name_fa)
    name_primer_f <- str_c(id, "-F")
    name_primer_r <- str_c(id, "-R")
    Primer <- c(Primer, name_primer_f, name_primer_r)
    Sequence <- c(Sequence, primer_F, primer_R)
  }
  Primer_Design <- data.frame(ID=Primer, Primer=Sequence, stringsAsFactors=FALSE)
  return(Primer_Design)
}


# write.csv(Primer_Design, "Primer.csv", row.names=FALSE)
