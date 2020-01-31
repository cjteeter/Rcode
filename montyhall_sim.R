##### Simulating the Monty Hall Problem - Switch or Stay? #####

# https://en.wikipedia.org/wiki/Monty_Hall_problem

# Load Necessary Libraries ------------------------------------------------
require(tidyverse)

# Create function to simulate problem, and print and plot results --------------
MontyHall <- function(switch = TRUE, n = 1000, seasons = 1) {
        
        # Setup data frame to record results --------------------------------------------------------------------
        outcome <- data.frame(Season = numeric(),
                              Strategy = character(),
                              Prize = character(),
                              Instances = numeric(),
                              Percent = numeric())
        
        # Allow for multiple 'seasons' of playing the game n times. ---------------
        for (s in 1:seasons) {
                
                # Setup vector to record the prize results of each iteration of the game (1 = car, 0 = goat) --------
                game_result <- NULL
                
                for (p in 1:n) {

                        # Set up components of the game -------------------------------------------
                        prizes <- c('Car', 'Goat1', 'Goat2')
                        door_setup <- sample(prizes, 3)
                        contestant_choice <- sample(1:3, 1)
                        door_reveal <- ifelse(door_setup[contestant_choice] == 'Car', 
                                              sample(door_setup[-c(contestant_choice)], 1), 
                                              door_setup[-c(contestant_choice, which(door_setup == 'Car'))])
                        if (switch) {
                                contestant_prize <- door_setup[-c(contestant_choice, which(door_setup == door_reveal))]
                                game_result[p] <- ifelse(contestant_prize == 'Car', 1, 0)
                        } else if (!switch) {
                                contestant_prize <- door_setup[contestant_choice]
                                game_result[p] <- ifelse(contestant_prize == 'Car', 1, 0) }
                }
                
                # Add results from this 'season' to the results data frame ------------------------
                tmp_outcome <- data.frame(Season = rep(s, 2),
                                     Strategy = ifelse(switch, rep('Switch', 2), rep('Stay', 2)),
                                     Prize = c('Car', 'Goat'), 
                                     Instances = c(sum(game_result), sum(!game_result)),
                                     Percent = c(round(sum(game_result)/n, 4)*100, round(sum(!game_result)/n, 4)*100))
                
                outcome <- rbind(outcome, tmp_outcome)
        }
        
        # Generate Summary Statistics for Plotting --------------------------------
        outcome_summary <- outcome %>%
                                group_by(Prize) %>%
                                summarise(Mean = round(mean(Percent, na.rm = T), 4),
                                          StDev = round(sd(Percent, na.rm = T), 4),
                                          CI = round((qnorm(0.975)*StDev)/sqrt(seasons), 4))
        
        # Plot Results ------------------------------------------------------------
        outcome_plot <- ggplot(outcome_summary, aes(x = Prize, y = Mean)) +
                                { if (seasons > 1) geom_errorbar(aes(ymin = (Mean - CI), ymax = (Mean + CI)), color = 'black', size =  1, width = 0) } +
                                geom_point(size = 4, stroke = 1.75, pch = 21, color = 'firebrick3', fill = 'white') +
                                scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
                                labs(x = "Contestant's Prize", y = if(seasons > 1) { 'Relative Frequency [+/- 95% CI]' } else { 'Relative Frequency' }, 
                                     subtitle = paste0("Results from ", {if(seasons > 1) { paste0(seasons, " runs of ") }}, n, 
                                                       " iterations of the Monty Hall Problem\nin which the contestant always",
                                                       ifelse(switch, " SWITCHES from their initial choice.\n", " STAYS with their initial choice.\n"))) +
                                theme_bw() +
                                theme(plot.subtitle = element_text(hjust = 1))
        
        return(list("data" = outcome, "summary" = outcome_summary, "plot" = outcome_plot))
}