#'
#' @title rtPCR result analysis
#'
#' @description calculate real-time generated .xls file, both delta-delta-CT and merting curve
#'
#' @importFrom dplyr filter group_by summarise
#'
#' @importFrom magrittr %>%
#'
#' @param data read expreesion CT values which is exported from Quantistudio
#'
#' @param choose_samples choose which samples need to be analysed
#'
#' @param choose_targets choose which genes need to be analysed
#'
#' @param na.do how to deal with Underteriminated genes
#'
#' @param real_file excel file path, used to extract merlting curve info
#'
#' @param skip_row skip lines when reading *real_file*
#'
#' @return a list contain multiple info
#'
#' @export
#'
rtPCR <- function(data, choose_samples, choose_targets, na.do, real_file, skip_row) {
    if(is.null(data))
        return()
    #    data <- read_excel(filepath, sheet='Results', col_names=TRUE, skip=41, na='NA')
    #    data <- data[, c(4,5,15)]
    #    colnames(data) <- c('Sample', 'Target', 'CT')
    #    sample_names <- unique(data$Sample)
    #    target_names <- unique(data$Target)

    data_choose <- data %>% filter(Sample %in% choose_samples, Target %in% choose_targets)
    data_choose$CT <- as.numeric(data_choose$CT)

    # deal with NAs
    if(isTRUE(na.do)) {
        x <- data_choose[complete.cases(data_choose), ] %>%
            group_by(Sample, Target) %>%
            summarise(num=n())

        samples.1 <- c()
        targets.1 <-c()
        samples.2 <- c()
        targets.2 <- c()
        for(i in 1:dim(x)[1]) {
            if(x$num[i]==1){
                sample.1 <- x$Sample[i]
                target.1 <- x$Target[i]
                samples.1 <- c(samples.1, sample.1)
                targets.1 <- c(targets.1, target.1)
            } else {
                if(x$num[i]==2) {
                    sample.2 <- x$Sample[i]
                    target.2 <- x$Target[i]
                    samples.2 <- c(samples.2, sample.2)
                    targets.2 <- c(targets.2, target.2)
                }
            }

        }

        na.record.samples <- union(samples.1, samples.2)
        na.record.targets <- union(targets.1, targets.2)

        if(length(samples.1)!=0) {
            for(i in 1:length(samples.1)) {
                block.1 <- data_choose %>% filter(Sample==samples.1[i], Target==targets.1[i])
                block.1[(is.na(block.1$CT)), 'CT'] <- block.1[!is.na(block.1$CT), 'CT']
                data_choose[data_choose$Sample==samples.1[i]& data_choose$Target==targets.1[i],]$CT <- block.1$CT
            }
        }

        if(length(samples.2)!=0) {
            for(i in 1:length(samples.2)) {
                block.2 <- data_choose %>% filter(Sample==samples.2[i], Target==targets.2[i])
                block.2[(is.na(block.2$CT)), 'CT'] <- mean(block.2[!is.na(block.2$CT), 'CT']$CT)
                data_choose[data_choose$Sample==samples.2[i]& data_choose$Target==targets.2[i],]$CT <- block.2$CT
            }
        }

    } else {
        data_choose <- data_choose[complete.cases(data_choose),]
        na.record.samples <- c(NULL)
        na.record.targets <- c(NULL)
    }




    ##############  relative expression

    refer_target_means <- c()
    for(s in choose_samples) {
        refer_target_mean <- filter(data_choose,Target==choose_targets[1], Sample==s)
        refer_target_mean <- mean(refer_target_mean$CT, na.rm=TRUE)
        refer_target_means <- c(refer_target_means, refer_target_mean)
    }


    de_ct <- list()
    for(t in 1:(length(choose_targets)-1)) {
        de_ct[[t]] <- list()

        for(s in 1:length(choose_samples)) {
            de_ct.1 <- filter(data_choose, Sample==choose_samples[s], Target==choose_targets[1])
            de_ct.2 <- filter(data_choose, Sample==choose_samples[s], Target==choose_targets[t+1])
            de_ct.diff <- de_ct.2$CT - mean(de_ct.1$CT)
            de_ct[[t]][[s]] <- de_ct.diff
        }
    }


    de_de_ct_2n_mean <- list()
    de_de_ct_2n_sd <- list()
    for(t in 1:(length(choose_targets)-1)) {
        de_de_ct_2n_mean[[t]] <- list()
        de_de_ct_2n_sd[[t]] <- list()

        for(s in 1:length(choose_samples)) {
            de_de_ct.1 <- mean(de_ct[[t]][[1]])
            de_de_ct.2 <- de_ct[[t]][[s]]
            diff <- de_de_ct.2 - de_de_ct.1
            values <- 2^(-diff)
            de_de_ct_2n_mean[[t]][[s]] <- mean(values)
            de_de_ct_2n_sd[[t]][[s]] <- sd(values)
        }
    }


    plot.samples <- c()
    plot.targets <- c()
    plot.values <- c()
    plot.sds <- c()
    i <- 1
    for(t in 1:(length(choose_targets)-1)) {

        for(s in 1:length(choose_samples)) {
            plot.sample <- choose_samples[s]
            plot.target <- choose_targets[t+1]
            plot.value <- de_de_ct_2n_mean[[t]][[s]]
            plot.sd <- de_de_ct_2n_sd[[t]][[s]]

            plot.samples <- c(plot.samples, plot.sample)
            plot.targets <- c(plot.targets, plot.target)
            plot.values <- c(plot.values, plot.value)
            plot.sds <- c(plot.sds, plot.sd)

            i <- i + 1
        }
    }


    data.plot <- data.frame(Samples=plot.samples, Targets=plot.targets, Values=plot.values, SDs=plot.sds, stringsAsFactors=FALSE)
    data.plot$Samples <- factor(data.plot$Samples, levels=unique(choose_samples))


    ## melt curve
    ############
    ############
    data_melt <- readxl::read_excel(real_file, sheet='Melt Curve Raw Data', col_names=TRUE, skip=skip_row)
    data_melt$Well <- as.character(data_melt$Well)
    data_melt$Target <- NA
    data_melt$Sample <- NA
    #  add target and sample value to melt curve data.frame
    for(i in choose_samples){
        data_melt$Sample[which(data_melt$Well %in% data_choose$Well[which(data_choose$Sample==i)])] <- i
    }

    for(i in choose_targets){
        data_melt$Target[which(data_melt$Well %in% data_choose$Well[which(data_choose$Target==i)])] <- i
    }
    # omit values that did'not calculted
    data_melt <- na.omit(data_melt)
    ## melt curve
    ############
    ############


    return(list(data.plot, na.record.samples, na.record.targets, data_melt))
}






