library(rvest)
library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(data.table)


header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
spotrac_web_scraper <- function(year){
  website <- paste("https://www.spotrac.com/nfl/contracts/sort-value/all-time/start-", year,  "/limit-3000/", sep = "")
  webpage <- read_html(website)
  
  tbls <- html_nodes(webpage, "table")
  
  
  tbls_ls <- webpage %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE)
  
  
  player_contract_info_df <- tbls_ls[[1]]
  # player_contract_info_df$fa_year <- 2021
  
  string_split_player_info_df <- str_split_fixed(player_contract_info_df$Player, "\n", 18)
  cleanup_df <- data.frame("playerName"=string_split_player_info_df[,8], "position"=string_split_player_info_df[,11], "FA_info"=string_split_player_info_df[,18])
  cleanup_df <- cleanup_df %>% separate(FA_info, c("a_omit", "start_year", "end_year", "d_omit"))
  cleanup_df$a_omit <- NULL
  cleanup_df$d_omit <- NULL
  
  player_contract_info_df$Player <- NULL
  player_contract_info_df$playerName <- cleanup_df$playerName
  player_contract_info_df$position <- cleanup_df$position
  player_contract_info_df$start_year <- as.numeric(cleanup_df$start_year)
  player_contract_info_df$end_year <- player_contract_info_df$start_year+player_contract_info_df$Yrs-1
  
  player_contract_info_df$Value <- as.numeric(gsub('[$,]', '', player_contract_info_df$Value))
  player_contract_info_df$AAV <- as.numeric(gsub('[$,]', '', player_contract_info_df$AAV))
  player_contract_info_df$`Sign Bonus` <- as.numeric(gsub('[$,]', '', player_contract_info_df$`Sign Bonus`))
  player_contract_info_df$`G'teed @ Sign` <- as.numeric(gsub('[$,]', '', player_contract_info_df$`G'teed @ Sign`))
  player_contract_info_df$`Practical G'teed` <- as.numeric(gsub('[$,]', '', player_contract_info_df$`Practical G'teed`))
  player_contract_info_df[is.na(player_contract_info_df)] <- 0
  player_contract_info_df$class <- year
  return(player_contract_info_df)
}

###This code can take a while to run, so usually it's best to run the loop one year at a time

m <- 1
fa_info_list <- list()
for (year in 2011:2012) {
  fa_info_df <- spotrac_web_scraper(year)
  fa_info_list[[m]] <- fa_info_df
  m <- m+1
}
concatenated_fa_info <- (rbindlist(fa_info_list))


all_fa_info <- concatenated_fa_info
all_fa_info$playerName  <- gsub("Jr.", '', all_fa_info$playerName)
all_fa_info$playerName  <- gsub("Jr", '', all_fa_info$playerName)
all_fa_info$playerName  <- gsub("Sr.", '', all_fa_info$playerName)
all_fa_info$playerName  <- gsub("III", '', all_fa_info$playerName)
all_fa_info$playerName  <- gsub("IV", '', all_fa_info$playerName)


all_fa_info<- (separate(all_fa_info, playerName, into = c("firstName", "lastName"), sep = " (?=[^ ]+$)"))
all_fa_info$firstName<- trimws(all_fa_info$firstName, which = c("left"))
all_fa_info$position<- trimws(all_fa_info$position, which = c("left"))
all_fa_info$start_year<- trimws(all_fa_info$start_year, which = c("left"))
all_fa_info$end_year<- trimws(all_fa_info$end_year, which = c("left"))
all_fa_info<- (separate(all_fa_info, firstName, into = c("firstName", "lastNameMerge1"), sep = " (?=[^ ]+$)"))
all_fa_info<- (separate(all_fa_info, firstName, into = c("firstName", "lastNameMerge2"), sep = " (?=[^ ]+$)"))
all_fa_info[is.na(all_fa_info)] <- ""
all_fa_info$lastName <- paste(all_fa_info$lastNameMerge2, all_fa_info$lastNameMerge1, all_fa_info$lastName)
all_fa_info$lastNameMerge1 <- NULL
all_fa_info$lastNameMerge2 <- NULL
all_fa_info$lastName <- gsub('\\s+', '', all_fa_info$lastName)
all_fa_info$pfr_nomenclature <- paste(substr(all_fa_info$lastName,1,4),substr(all_fa_info$firstName,1,2), sep="")

all_fa_info$start_year <- as.numeric(all_fa_info$start_year)
all_fa_info$end_year <- as.numeric(all_fa_info$end_year)

all_fa_info <- all_fa_info[all_fa_info$start_year!=0,]

player_pfr_df <- data.frame(Doubles=double())

player_pfr_info_list <- list()
for (row_indexer in 1:nrow(all_fa_info)) {
  ind_fa_info <- all_fa_info[row_indexer,]

  player_pfr_df <- mtcars[FALSE,]
  p <-0
  
  while (nrow(player_pfr_df)==0 & p<100) {
    ln_initial <- substr(ind_fa_info$lastName,1,1)
    pfr_id_1 <- p-round_any(p, 10, f=floor)
    pfr_id_2 <- round_any(p/10, 1, f=floor)
    pfr_id <- paste(pfr_id_2, pfr_id_1, sep="")
    
    
    website <- paste("https://www.pro-football-reference.com/players/", ln_initial, "/", ind_fa_info$pfr_nomenclature, pfr_id,".htm",sep = "")
    tryCatch({
      webpage <- read_html(website)
      # print(webpage)
      }, error=function(e){})
    
    
    

    all_text <- webpage %>%
      html_nodes("p") %>%
      html_text()
    name_string <- strsplit(all_text[1], "\n")[[1]][3]
    name_string  <- gsub("Jr.", '', name_string)
    name_string  <- gsub("Jr", '', name_string)
    name_string  <- gsub("Sr.", '', name_string)
    name_string  <- gsub("III", '', name_string)
    name_string  <- gsub("IV", '', name_string)
    name_string <- trimws(name_string)

    
    tbls <- html_nodes(webpage, "table")


    tbls_ls <- webpage %>%
      html_nodes("table") %>%
      .[1] %>%
      html_table(fill = TRUE)
    player_pfr_df <- rbindlist(tbls_ls)
    
    if (nrow(player_pfr_df)>0) {
      if (colnames(player_pfr_df)[1]=="V1") {
        player_pfr_df <- header.true(player_pfr_df)
      }
      player_pfr_df <- player_pfr_df[-nrow(player_pfr_df),]
      player_pfr_df$Year<-gsub("\\*","",as.character(player_pfr_df$Year))
      player_pfr_df$Year <- as.numeric(player_pfr_df$Year)
      player_pfr_df <- player_pfr_df[!is.na(player_pfr_df$Year), ]


      if (min(player_pfr_df$Year) > unique(ind_fa_info$start_year)) {
        player_pfr_df <- mtcars[FALSE,]
      } else if (max(player_pfr_df$Year)<ind_fa_info$start_year) {
        player_pfr_df <- mtcars[FALSE,]
      }
      
      pfr_firstName <- substr(name_string,1,nchar(ind_fa_info$firstName))
      pfr_lastName <- substrRight(name_string, nchar(ind_fa_info$lastName))
      
      # if (pfr_firstName!=(ind_fa_info$firstName)) {
      #   player_pfr_df <- mtcars[FALSE,]
      # } 
      if (pfr_lastName!=(ind_fa_info$lastName)) {
        player_pfr_df <- mtcars[FALSE,]
      }
      
    }
    p <- p+1

  }
  
  relevant_info_df <- data.frame(Year=player_pfr_df$Year, AV=player_pfr_df$AV)
  player_pfr_info_list[[row_indexer]] <- relevant_info_df
  print(row_indexer)
}


iterative_list <- list()
for (list_indexer in 1:length(player_pfr_info_list)) {
  temp_df <- player_pfr_info_list[[list_indexer]]
  print(list_indexer)
  if (nrow(temp_df)>0) {
    temp_df$iteration_id <- list_indexer
  } else {
    temp_df$Year <- numeric()
    temp_df$AV <- numeric()
    temp_df$iteration_id <- numeric()
  }
  iterative_list[[list_indexer]] <- temp_df
}

player_av_20303_to_23351_df <- (rbindlist(iterative_list))
write.csv(player_av_20303_to_23351_df,"C:/Users/pauli/OneDrive - The University of Chicago/Documents/NFL Research/VCG Compensatory Picks\\player_av_20303_to_23351_df.csv", row.names = FALSE)



comp_pick_file_path <- "C:/Users/pauli/OneDrive - The University of Chicago/Documents/NFL Research/VCG Compensatory Picks"
setwd(comp_pick_file_path)
player_av_1_to_3362_df <- read.csv("player_av_1_to_3362_df.csv")
player_av_3363_to_9220_df <- read.csv("player_av_3363_to_9220_df.csv")
player_av_9221_to_13325_df <- read.csv("player_av_9221_to_13325_df.csv")
player_av_13326_to_16591_df <- read.csv("player_av_13326_to_16591_df.csv")
player_av_19399_to_20302_df <- read.csv("player_av_19399_to_20302_df.csv")
player_av_20303_to_23351_df <- read.csv("player_av_20303_to_23351_df.csv")

all_fa_info$iteration_id <- 1:nrow(all_fa_info)
player_av_df <- do.call("rbind", list(player_av_1_to_3362_df, player_av_3363_to_9220_df, player_av_9221_to_13325_df,
                     player_av_13326_to_16591_df, player_av_19399_to_20302_df, player_av_20303_to_23351_df))





