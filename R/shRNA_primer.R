#'
#' @importFrom stringr str_c
#'
#' @importFrom Biostrings DNAString DNAStringSet reverseComplement
#'
shRNA_primer <- function(splashRNA, vector)
{
  loop <- DNAString("TAGTGAAGCCACAGATGTA")
  overlap_5 <- DNAString("TGCTGTTGACAGTGAGCG")
  overlap_3 <- DNAString("TGCCTACTGCCTCGGACT")
  multi_antisense_guide <- splashRNA$Antisense

  Primer <- c()
  Sequence <- c()
  Construct <- list()
  for(i in 1:length(multi_antisense_guide))
  {
    antisense_guide <- DNAString(multi_antisense_guide[[i]])
    mismatch_base <- DNAString(switch(as.character(antisense_guide[22]),
                                      "A"="C","G"="A","C"="A","T"="C"))
    sense_guide <- append(mismatch_base, reverseComplement(antisense_guide[-22]))
    insert_fragment <- c(sense_guide, loop, antisense_guide)
    new_fullseq <- Biostrings::replaceLetterAt(vector, 5424:5486, insert_fragment)
    primer_F <- as.character(str_c(loop, antisense_guide, overlap_3))
    primer_R <- as.character(reverseComplement(
      DNAString(str_c(overlap_5, sense_guide, loop))))
    id <- splashRNA$ID[i]

    name_primer_f <- str_c(id, "-F")
    name_primer_r <- str_c(id, "-R")
    Primer <- c(Primer, name_primer_f, name_primer_r)
    Sequence <- c(Sequence, primer_F, primer_R)

    # name_fa <- str_c(id, ".fa")
    # Construct <- rlist::list.append(new_construct_all, DNAStringSet(new_fullseq), name_fa)
  }

  Primer <- data.frame(ID=Primer, Primer=Sequence, stringsAsFactors=FALSE)

  return(Primer)
}
