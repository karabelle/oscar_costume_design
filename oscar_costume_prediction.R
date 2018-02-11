# re-doing everything tidy
library(dplyr)
library(readr)
library(ggplot2)

# insider weights
insider_awards <- c("BAFTA", "CDGA-C", "CDGA-F", "CDGA-P")

# read in file
costume_file <- readr::read_csv('costume_dataset.csv')

# get past Oscar winners - and we really only care about their names
past_oscar_winners <- costume_file %>%
                            filter(Award == "Oscar", Winner == "Y") %>%
                            select(Movie)

# how did those movies do in other award ceremonies?
oscar_winners_award_count <- costume_file %>%
                                semi_join(past_oscar_winners, by = "Movie") %>%
                                filter(Award != "Oscar") %>%
                                group_by(Award) %>%
                                summarise(wins = sum(Winner == "Y"), noms = n())

# how many times have these other awards been given out? (get from larger costume set)
# I know the answer to this, since I gathered the data - but let's pretend we got this data from somewhere else
# join these calculations together
# calculate win %, squared win %, and final, insider-weighted total * 100 (see fivethirtyeight Oscar prediction methodology)

award_pts <- costume_file %>%
                filter(Year < 2017, Award != "Oscar") %>%
                group_by(Award) %>%
                summarise(awards = n_distinct(Year)) %>%
                full_join(oscar_winners_award_count, by = "Award") %>%
                mutate(win_pct = wins / awards, sq_win_pct = win_pct ^ 2, wt_win_pct = if_else(Award %in% insider_awards, 2, 1) * sq_win_pct * 100)

# now we need to go back and calculate the scores for our Oscar nominees (and past winners)
oscar_nominees <- costume_file %>%
                    filter(Award == "Oscar", is.na(Winner)) %>%
                    select(Movie)

oscar_nominees_other_awards <- costume_file %>%
                                semi_join(oscar_nominees, by = "Movie") %>%
                                filter(Award != "Oscar") %>%
                                group_by(Movie, Award) %>%
                                summarise(wins = sum(Winner == "Y"), noms = n()) %>%
                                left_join(award_pts, by = "Award") %>%
                                select(Movie, Award, ends_with(".x"), wt_win_pct) %>%
                                mutate(total_pts = wins.x * wt_win_pct + noms.x * wt_win_pct * 0.2) %>%
                                group_by(Movie) %>%
                                summarise(curr_total = sum(total_pts, na.rm = TRUE)) %>%
                                arrange(desc(curr_total))
                    
# woot! re-did that original mess
# to reproduce the visuals from fivethirtyeight we also need the scores for our past_oscar_winners
# CCA - only awarded 2009+ adds a lot of points - 2 choices: only calc comparison scores for 2009+ or pretend all pre-2009 winners got those points

oscar_winners_other_awards <- costume_file %>%
                                semi_join(past_oscar_winners, by = "Movie") %>%
                                filter(Award != "Oscar", Year >= 2009) %>%
                                group_by(Movie, Award) %>%
                                summarise(wins = sum(Winner == "Y"), noms = n()) %>%
                                left_join(award_pts, by = "Award") %>%
                                select(Movie, Award, ends_with(".x"), wt_win_pct) %>%
                                mutate(total_pts = wins.x * wt_win_pct + noms.x * wt_win_pct * 0.2) %>%
                                group_by(Movie) %>%
                                summarise(win_total = sum(total_pts, na.rm = TRUE)) %>%
                                arrange(desc(win_total))

# fivethirtyeight didn't look at this, but how did past losers do?
past_oscar_losers <- costume_file %>%
                        filter(Award == "Oscar", Winner == "N") %>%
                        select(Movie)

oscar_losers_other_awards <- costume_file %>%
                              semi_join(past_oscar_losers, by = "Movie") %>%
                              filter(Award != "Oscar", Year >= 2009) %>%
                              group_by(Movie, Award) %>%
                              summarise(wins = sum(Winner == "Y"), noms = n()) %>%
                              left_join(award_pts, by = "Award") %>%
                              select(Movie, Award, ends_with(".x"), wt_win_pct) %>%
                              mutate(total_pts = wins.x * wt_win_pct + noms.x * wt_win_pct * 0.2) %>%
                              group_by(Movie) %>%
                              summarise(win_total = sum(total_pts, na.rm = TRUE)) %>%
                              arrange(desc(win_total))

# looks like Jackie got robbed (by Fantastic Beasts) according to this model!
# a little troubling that that happened in the most recent year...

# time to build that visual
# 1. Stacked bar chart by Movie (for our nominees)
# Movie, Award, Winner, Points

oscar_nominees_for_chart <- costume_file %>%
                              semi_join(oscar_nominees, by = "Movie") %>%
                              filter(Award != "Oscar") %>%
                              left_join(award_pts, by = "Award") %>%
                              select(Movie, Award, Winner, wt_win_pct) %>%
                              mutate(Winner = ifelse(is.na(Winner), "N", Winner), points = wt_win_pct * if_else(Winner == "Y", 1, 0.2)) %>%
                              bind_rows(tibble(Movie = 'Past Winners', Award = NA, Winner = NA, wt_win_pct = NA, points = NA)) %>%
                              group_by(Movie) %>%
                              mutate(total_points = round(max(cumsum(points))), 1) %>%
                              arrange(desc(total_points))

ggplot(data = oscar_nominees_for_chart, aes(x = reorder(Movie, total_points, FUN = max), points, group = Award)) + 
      geom_col(aes(fill = factor(Winner)), color = "black", show.legend = FALSE) + 
      scale_fill_manual(values = c("N" = "grey", "Y" = "yellow")) +
      geom_text(mapping = aes(label = Award), size = 2, position = position_stack(vjust = 0.5), angle = 90) + 
      geom_text(mapping = aes(x = reorder(Movie, total_points, FUN = max), total_points + 5, group = Movie, label = total_points), size = 3) +
      coord_flip() +
      xlab("") + ylab("") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank()) +
      geom_hline(mapping = aes(yintercept = min(win_total)), data = oscar_winners_other_awards, linetype = 3) +
      geom_point(mapping = aes(x = factor("Past Winners"), y = win_total, group = Movie), data = oscar_winners_other_awards) +
      annotate(geom = "text", x = factor("Past Winners"), y = 124.6, label = "Fantastic Beasts and Where to Find Them: 125 pts", vjust = -0.5, size = 3) +
      annotate(geom = "text", x = factor("Past Winners"), y = 314, label = "Anna Karenina: 314 pts", vjust = -0.5, size = 3) +
      ylim(NA, 330)