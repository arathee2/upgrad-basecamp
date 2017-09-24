library(tidyverse)
library(data.table)

# load data
ipl <- fread("deliveries.csv")
str(ipl)

# 1. Which batsman has scored the most number of runs against Royal Challengers Bangalore?
# Answer - G Gambhir (616 runs)
ipl %>% 
  filter(bowling_team == "Royal Challengers Bangalore") %>%
  group_by(batsman) %>% 
  summarise(batsman_runs = sum(batsman_runs)) %>%
  arrange(desc(batsman_runs)) %>% 
  top_n(5)

# 2. Which team has taken most number of wickets against "Rajasthan Royals"?
# Answer - Chennai Super Kings (107 wickets)
ipl %>% 
  filter(batting_team == "Rajasthan Royals") %>%
  group_by(bowling_team) %>% 
  summarise(wickets_taken = sum(is_wicket)) %>%
  arrange(desc(wickets_taken))

# 3. Which team has played the most number of IPL matches?
# Answer -  Mumbai Indians (140 matches)
ipl %>% 
  filter(inning == "1" | inning == "2") %>%
  group_by(match_id, batting_team) %>%
  summarise(balls_played = n()) %>%
  group_by(batting_team) %>%
  summarise(matches_played = n()) %>%
  arrange(desc(matches_played))

# 4. Which player has the most number of catches?
# Answer - KD Karthik (80 catches)
ipl %>% 
  filter(dismissal_kind == "caught") %>% 
  group_by(fielder) %>% 
  summarise(catches_taken = n()) %>% 
  arrange(desc(catches_taken)) %>%
  top_n(5)

# 5. Which bowler has bowled the most number of wides in the entire IPL?
# Answer - Lasith Malinga and Praveen Kumar (94 wides each)
ipl %>% 
  filter(wide_runs == "1") %>% 
  group_by(bowler) %>%
  summarise(total_wides = sum(wide_runs)) %>%
  arrange(desc(total_wides)) %>%
  top_n(10)

# 6. Which team has conceded the most number of runs as extras in the entire IPL?
# Answer - Mumbai Indians (1260 runs as extras)
ipl %>% 
  group_by(bowling_team) %>%
  summarise(extras_conceded = sum(extra_runs)) %>%
  arrange(desc(extras_conceded)) %>% 
  top_n(5)

# 7. Which batsman hit the most number of sixes against "SL Malinga"?
# Answer - SR Watson (5 sixes)
ipl %>% 
  filter(bowler == "SL Malinga" & batsman_runs == "6") %>%
  group_by(batsman) %>%
  summarise(num_sixes = n()) %>%
  arrange(desc(num_sixes)) %>% 
  top_n(5)

# 8. Which team has scored the most number of runs in a super over?
# Answer - Sunrisers Hyderabad (20 runs)
ipl %>% 
  filter(is_super_over == "1") %>%
  group_by(match_id, batting_team) %>%
  summarise(runs_scored = sum(total_runs)) %>%
  arrange(desc(runs_scored))

# 9. Which team has been in most super overs?
# Answer - Rajasthan Royals (3 matches)
ipl %>% 
  filter(is_super_over == "1") %>%
  group_by(match_id, batting_team) %>% 
  summarise(balls_played = n()) %>%
  group_by(batting_team) %>%
  summarise(matches_played = n()) %>%
  arrange(desc(matches_played))

# 10. Which batsman has been run out the most number of times?
# Answer - G Gambhir (15 times)
ipl %>% 
  filter(dismissal_kind == "run out") %>%
  group_by(player_dismissed) %>%
  summarise(times_run_out = n()) %>% 
  arrange(desc(times_run_out))

# 11. Who is the leading run scorer in IPL?
# Answer - V Kohli (4115 runs)
ipl %>% 
  group_by(batsman) %>%
  summarise(runs_scored = sum(batsman_runs)) %>%
  arrange(desc(runs_scored)) %>% 
  top_n(5)

# 12. Which bowler has picked up the most number of wickets against Delhi Daredevils?
# Answer - R Ashwin (21 wickets)
ipl %>% 
  filter(batting_team == "Delhi Daredevils") %>%
  group_by(bowler) %>% 
  summarise(wickets_taken = sum(is_wicket)) %>%
  arrange(desc(wickets_taken))

# 13. Which bowler is the leading wicket-taker in the IPL?
# Answer - SL Malinga (159 wickets) and DJ Bravo (137 wickets)
ipl %>%
  group_by(bowler) %>%
  summarise(wickets_taken = sum(is_wicket)) %>%
  arrange(desc(wickets_taken)) %>%
  top_n(5)

# 14. Which batsman hit the most number of fours in a single inning?
# Answer - PC Valthaty and AB de Villiers (19 fours each)
ipl %>% 
  filter(total_runs == "4") %>%
  group_by(match_id, inning, batsman) %>%
  summarise(num_fours = n()) %>% 
  arrange(desc(num_fours)) %>% 
  top_n(5)

# 15. Which player has the highest strike rate?
# Answer - CR Brathwaite (207.5 strike rate)
ipl %>% 
  group_by(batsman) %>%
  summarise(balls_played = n(), runs_scored = sum(batsman_runs)) %>%
  mutate(strike_rate = 100*runs_scored/balls_played) %>%
  arrange(desc(strike_rate)) %>% 
  top_n(5)


# 16. How many matches have witnessed the fall of all 20 wickets?
# Answer - 1 match (match_id - 151)
ipl %>% 
  group_by(match_id) %>% 
  summarise(total_wickets = sum(is_wicket)) %>%
  arrange(desc(total_wickets)) %>% 
  top_n(10)

# 17. Which was the losing team of the highest scoring match of IPL?
# Answer - Rajasthan Royals (CSK-246 vs RR-223)
ipl %>% 
  group_by(match_id) %>% 
  summarise(match_runs = sum(total_runs)) %>%
  arrange(desc(match_runs))

ipl %>% 
  filter(match_id == "147") %>%
  group_by(batting_team) %>% 
  summarise(team_runs = sum(total_runs))

# 18. Which Pune Warriors batsman has faced the most number of balls for the team?
# Answer - RV Uthappa (957 balls faced)
ipl %>% 
  filter(batting_team == "Pune Warriors") %>%
  group_by(batsman) %>% 
  summarise(balls_played = n()) %>%
  arrange(desc(balls_played)) 

# 19. Which team has scored the most number of runs in the entire IPL?
# Answer - Mumbai Indians (21721 runs)
ipl %>% 
  group_by(batting_team) %>%
  summarise(num_wickets = sum(is_wicket), runs_scored = sum(total_runs)) %>%
  arrange(desc(runs_scored))

# 20. Which team hit the most boundaries (fours and sixes) in a single inning?
# Answer - Royal Challengers Bangalore (41 boundaries) (match_id - 352)
ipl %>% 
  filter(total_runs == "4" | total_runs == "6") %>%
  group_by(match_id, inning, batting_team) %>%
  summarise(boundaries_hit = n()) %>% 
  arrange(desc(boundaries_hit)) %>% 
  top_n(5)

# 21. Which batsman has faced the most number of dot balls in an inning?
# Answer - DJ Bravo (29 dot balls faced)
ipl %>% 
  filter(total_runs == "0") %>% 
  group_by(match_id, inning, batsman) %>%
  summarise(dot_balls_faced = n()) %>% 
  arrange(desc(dot_balls_faced)) %>%
  top_n(10)


