#(3adv) --madrs best sx loader------
ketu <- read.csv("C:/Users/Manivel/Desktop/Pricelab/ketamine_symptoms/coredata/KetaminePlusCognitiv_SPSS-2021-11-05_MADRS_items_wide_forShabnam.csv")
grepx(colnames(ketu), pattern = c("preinfusion|24hr|day_5"))
kpc_short <- kpc[,c("partid", "KetDummyCoded", "Group3Way")]
colnames(kpc_short)[1] <- c("id")
qlist <- paste0("q", c(1:10))
ksx_madrs_sx_df <- list()
for (a in 1:length(qlist)){
	ketu_temp <- ketu[,c("id", colnames(ketu)[grep(colnames(ketu), pattern=paste0(qlist[a], "\\."))])]
	ketlt <- tidyr::gather(ketu_temp,"day","value",2:7)   
	ketlt$day <- gsub(pattern = ".*\\.", "\\1", x = ketlt$day)
	ketlt$day <- gsub(pattern = "_arm_1", replacement = "", x = ketlt$day)
	day_key <- data.frame(  col_1 = c("preinfusion", "24hr", "day_5", "day_12", "day_21", "day_30") , col_2 = c(0, 1, 5, 12, 21, 30) , stringsAsFactors=F)
	ketlt$day <- varmatch_generic(ketlt$day,df_key = day_key)
	ketlt <- left_join(ketlt, kpc_short,by="id")
	ketlt$quest_id <- qlist[a]
	q_key <- data.frame(  col_1 = c("q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10") , col_2 = c("Reported_Sadness", "Apparent_Sadness", "Inner_Tension", "Reduced_Sleep", "Reduced_Appetite", "Concentration_difficulties", "Lassitude", "Inability_to_Feel", "Pessimistic_Thoughts", "Suicidal_Thoughts") , stringsAsFactors=F)
	ketlt$sx <- varmatch_generic(ketlt$quest_id, q_key)
	ketlt$partid <- ketlt$id
	ketlt$timepoint <- ketlt$day
	ksx_madrs_sx_df[[a]] <- ketlt
}
ksx_madrs <- do.call("rbind", ksx_madrs_sx_df)
madrs_sx <- names(table(ksx_madrs$sx))

