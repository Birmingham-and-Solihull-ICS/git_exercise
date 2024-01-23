library(NHSRdatasets)
library(tidyverse)

# Load demo data
data("ae_attendances")

#sub set to UHB type 1
sub <- 
  ae_attendances %>% 
  filter(org_code %in% c("RRK", "RR1") & type == 1) %>% 
  # merge HEFT and UHB
  mutate(org_code = "RRK") %>% 
  group_by(period, org_code, type) %>% 
  summarise(attendances = sum(attendances)
            , breaches = sum(breaches)
            , admissions = sum(admissions)) %>% 
  ungroup()



# Visualise attendance
ggplot(sub, aes(y=attendances, x=period))+
  geom_point()+
  geom_line()


# Exercise:  Now plot attendance, breach and admissions on the same plot(s).
# Your style, do it as you like, but make it look nice...

# CLUE:  pivoting is helpful here... 

