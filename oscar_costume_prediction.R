#read in file
costume_file = read.csv('costume_dataset.csv', as.is = 2)

#get past Oscar winners
oscar_winners = costume_file[costume_file$Award == "Oscar" & costume_file$Winner == "Y",]$Movie

#get matching wins by award & year
other_wins = costume_file[costume_file$Movie %in% oscar_winners & costume_file$Winner == "Y" & costume_file$Award != "Oscar",]
#count rows per award across years
win_count = aggregate(other_wins$Winner, list(award_name = other_wins$Award), length)
#count rows in original table (only wins, same years)
non_oscar_wins = costume_file[costume_file$Award != "Oscar" & costume_file$Winner == "Y" & costume_file$Year %in% unique(other_wins$Year),]
all_count = aggregate(non_oscar_wins$Winner, list(award_name = non_oscar_wins$Award), length)

summary_table = merge(win_count, all_count, by = "award_name", all = TRUE)
colnames(summary_table) <- c("award_name", "pred_wins", "poss_wins")
summary_table$pred_wins[is.na(summary_table$pred_wins)] <- 0

#Give CDGA-FP credit to both CDGA-F & CDGA-P


#get our correct prediction pct
summary_table$win_pct <- summary_table$pred_wins / summary_table$poss_wins
#square our pct
summary_table$sq_pct <- summary_table$win_pct^2
#apply weights to our pct based on which awards are "insider" awards
insider_wts = data.frame(c("BAFTA", "CDGA-C", "CDGA-F", "CDGA-P"), 2)