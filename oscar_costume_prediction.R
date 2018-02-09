#we might need this
library(reshape2)

#read in file
costume_file <- read.csv('costume_dataset.csv', as.is = c(2, 4))

#get past Oscar winners
oscar_winners = costume_file[costume_file$Award == "Oscar" & costume_file$Winner == "Y",]$Movie

#get matching wins by award & year
other_wins <- costume_file[costume_file$Movie %in% oscar_winners & costume_file$Winner == "Y" & costume_file$Award != "Oscar",]
#count rows per award across years
win_count = aggregate(other_wins$Winner, list(award_name = other_wins$Award), length)
#count rows in original table (only wins, same years)
non_oscar_wins <- costume_file[costume_file$Award != "Oscar" & costume_file$Winner == "Y" & costume_file$Year %in% unique(other_wins$Year),]
all_count <- aggregate(non_oscar_wins$Winner, list(award_name = non_oscar_wins$Award), length)

summary_table <- as.data.frame(merge(win_count, all_count, by = "award_name", all = TRUE))
colnames(summary_table) <- c("award_name", "pred_wins", "poss_wins")
summary_table$pred_wins[is.na(summary_table$pred_wins)] <- 0

#replaced with what they could reasonably be expected to be categorized now
#REMOVED CDGA-FP values -Give CDGA-FP credit to both CDGA-F & CDGA-P
#can just add numerator and denominator to both
# rownames(summary_table) <- summary_table$award_name
# summary_table <- t(summary_table[,2:3])
# summary_table[,"CDGA-F"] <- summary_table[,"CDGA-F"]+summary_table[,"CDGA-FP"]
# summary_table[,"CDGA-P"] <- summary_table[,"CDGA-P"]+summary_table[,"CDGA-FP"]
# summary_table <- as.data.frame(summary_table)
# summary_table <- within(summary_table, rm("CDGA-FP"))
# summary_table <- as.data.frame(t(summary_table))

#get our correct prediction pct
summary_table$win_pct <- summary_table$pred_wins / summary_table$poss_wins

#square our pct
summary_table$sq_pct <- summary_table$win_pct^2
#apply weights to our pct based on which awards are "insider" awards
insider_awards <- c("BAFTA", "CDGA-C", "CDGA-F", "CDGA-P")
#this is so incredibly ugly, there is no way this is the correct way to do this
summary_table[rownames(summary_table) %in% insider_awards,]$sq_pct <- summary_table[rownames(summary_table) %in% insider_awards,]$sq_pct*2

#ok, now we need to apply these weights back to the results for our Oscar films to get their scores
#and we might as well get rid of a couple variables we aren't using
rm("all_count", "win_count", "non_oscar_wins", "other_wins")

#we need a table which has all the results for our oscar winning films
#then we can go through and if its a win, give the points - if its a nom, give 1/5 the points
#want movie titles on rows, and award names on columns - Not really, easier the other way
#apply a formula which calculates by row based on result and column name
oscar_results <- within(costume_file[costume_file$Movie %in% oscar_winners & costume_file$Award != "Oscar",], rm("Year"))
#would make everything easier to convert Win column to 1 and 0
oscar_results$Winner[oscar_results$Winner == "Y"] <- 1
oscar_results$Winner[oscar_results$Winner == "N"] <- 0.2
oscar_results$Winner <- as.numeric(oscar_results$Winner)
oscar_results <- dcast(oscar_results, Award ~ Movie)

#don't use CDGA-C because we know that it has no eventual oscar winners
total_pts <- as.data.frame(colSums(summary_table["award_name" != "CDGA-C",]$sq_pct * oscar_results[,-1], na.rm = TRUE))
#ok, so now we have the points for all our historic wins!
colnames(total_pts) = "tpts"
total_pts$movie_name <- rownames(total_pts)
rownames(total_pts) <- c(1:dim(total_pts)[1])
#apparently they're prettier this way
total_pts$tpts <- total_pts$tpts * 100

#ok, so what about our contenders for this year?
oscar_contenders <- costume_file[costume_file$Award == "Oscar" & costume_file$Winner == "",]$Movie
#what awards have they earned?
contender_results <- within(costume_file[costume_file$Movie %in% oscar_contenders & costume_file$Award != "Oscar",], rm("Year"))
#ok, blanks mean that they were nominated and winner isn't known, so assume they all lost
contender_results$Winner[contender_results$Winner == ""] <- "N"
#sigh, we should just convert the winner column to 0 and 1 at the beginning
contender_results$Winner[contender_results$Winner == "Y"] <- 1
contender_results$Winner[contender_results$Winner == "N"] <- 0.2
contender_results$Winner <- as.numeric(contender_results$Winner)

#in fact, this whole thing should be a function where we grab the results and tally the score!!!! TODO
#because we're repeating ourselves and we're going to want to do this to - Oscar winners & contenders through history
contender_results <- dcast(contender_results, Award ~ Movie)
#don't use CDGA-C because we know that it has no eventual oscar winners
current_pts <- as.data.frame(colSums(summary_table["award_name" != "CDGA-C",]$sq_pct * contender_results[,-1], na.rm = TRUE))
colnames(current_pts) = "cpts"
current_pts$movie_name <- rownames(current_pts)
rownames(current_pts) <- c(1:dim(current_pts)[1])
#apparently they're prettier this way
current_pts$cpts <- current_pts$cpts * 100

#ok, so next we visualize!
#and then go back and make this all way less ugly (also analyze past oscar non-winners!)

