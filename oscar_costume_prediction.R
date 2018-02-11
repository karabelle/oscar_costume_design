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
                              mutate(points = wt_win_pct * if_else(Winner == "Y", 1, 0.2))

ggplot(data = oscar_nominees_for_chart, aes(Movie, points, group = Award)) + 
      geom_col(aes(fill = factor(Winner)), color = "black", show.legend = FALSE) + 
      scale_fill_manual(values = c("N" = "grey", "Y" = "yellow")) +
      geom_text(mapping = aes(label = Award), size = 2, position = position_stack(vjust = 0.5)) + 
      coord_flip()

# still need to:
# 0 - figure out how to sort the movies so highest total on top
# 1 - Get rid of x-axis, y-axis label, and grid
# 2 - plot past winners' totals on the bottom as dots
# 3 - add dashed reference line for lowest winning score