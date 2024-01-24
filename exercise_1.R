library(NHSRdatasets)
library(tidyverse)
library(lubridate)
library(janitor)
library(plotly)


# Load demo data
data("ae_attendances") 

print(unique(ae_attendances$org_code))

print(unique(ae_attendances$type))

#sub set to UHB type 1
sub <- ae_attendances %>% 
  filter(org_code %in% c("RRK", "RR1") & type == '1') %>% 
  # merge HEFT and UHB
  mutate(org_code = "RRK") %>% 
  group_by(period, org_code, type) %>% 
  summarise(attendances = sum(attendances)
            , breaches = sum(breaches)
            , admissions = sum(admissions)) %>% 
  ungroup() %>% 
  clean_names(case = "upper_camel")



# Visualise attendance
ggplot(sub, aes(y=Attendances, x=Period))+
  geom_point()+
  geom_line()


# Line chart: visualize attendances over time 

plot_ly(sub,
        type = 'scatter',
        mode = 'lines') %>% 
  add_trace(x = ~Period,
            y = ~Attendances) %>% 
  layout(showlegend = 'F',
         title = 'Time Series of Type 1 A&E Attendances',
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector = list(
                        buttons = list(
                          list(count = 1, label = "1m", step = "month", stepmode = "backward"),
                          list(count = 6, label = "6m", step = "month", stepmode = "backward"),
                          list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
                          list(count = 1, label = "1y", step = "year", stepmode = "backward"),
                          list(step = "all")
                        )
                      ))) %>% 
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', margin = 0.1, width = 900)

# Area chart: visualize both attendances & admissions over time

plot_ly(sub) %>%
  add_trace(x = ~Period, y = ~Attendances, type = 'scatter', mode = 'lines', name = 'Attendances',
            fill = 'tozeroy') %>%
  add_trace(x = ~Period, y = ~Admissions, type = 'scatter', mode = 'lines', name = 'Admissions',
            fill = 'tozeroy') %>%
  layout(showlegend = TRUE,
         title = 'Time Series of Type 1 A&E Attendances and Admissions',
         xaxis = list(rangeslider = list(visible = TRUE),
                      rangeselector = list(
                        buttons = list(
                          list(count = 1, label = "1m", step = "month", stepmode = "backward"),
                          list(count = 6, label = "6m", step = "month", stepmode = "backward"),
                          list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
                          list(count = 1, label = "1y", step = "year", stepmode = "backward"),
                          list(step = "all")
                        )
                      )),
         plot_bgcolor = '#e5ecf6') %>%
  layout(xaxis = list(zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
         yaxis = list(title = '',
                      zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
         margin = list(l = 50, r = 50, t = 100, b = 50), width = 900)

# Stacked bar chart: visualize both attendances & admissions on each day of week

sub <- sub %>% 
  mutate(ProportionAdmitted = (Admissions / Attendances) * 100) %>% 
  mutate(ArrivalDay = wday(Period, label = TRUE))

sub2 <-  sub %>% 
  group_by(ArrivalDay) %>% 
  summarise(Attendances = sum(Attendances),
            Admissions = sum(Admissions))

plot_ly(sub2) %>%
  add_trace(x = ~ArrivalDay,
            y = ~Attendances,
            type = 'bar',
            name = 'Attendances') %>%
  add_trace(x = ~ArrivalDay,
            y = ~Admissions,
            type = 'bar',
            name = 'Admissions') %>%
  layout(showlegend = TRUE,
         title = list(text = 'Count of Attendances & Admissions by Arrival Day',
                      x = 0.5),
         yaxis = list(zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff',
                      title = 'Count'),
         xaxis = list(zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff',
                      title = 'Arrival Day'),
         margin = list(l = 50, r = 50, t = 100, b = 50), width = 900,
         plot_bgcolor = '#e5ecf6',
         barmode = 'stack')

# Line chart : Proportions of Admissions per attendance over time
plot_ly(sub,
        type = 'scatter',
        mode = 'lines') %>% 
  add_trace(x = ~Period,
            y = ~round(ProportionAdmitted,2),
            line = list(color = 'rgba(0,100,80,0.8)')) %>% 
  layout(showlegend = 'F',
         title = 'Proportion of Admissions per Type 1 A&E Attendance',
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector = list(
                        buttons = list(
                          list(count = 1, label = "1m", step = "month", stepmode = "backward"),
                          list(count = 6, label = "6m", step = "month", stepmode = "backward"),
                          list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
                          list(count = 1, label = "1y", step = "year", stepmode = "backward"),
                          list(step = "all")
                        )
                      ))) %>% 
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff',
                 title = "Proportion Admitted (%)"),
    plot_bgcolor='#e5ecf6', margin = 0.1, width = 900)

# Line chart: UHB vs BWCH 4 hour breaches 

sub3 <- ae_attendances %>% 
  filter(org_code %in% c("RRK", "RR1", "RQ3") & type == '1') %>% 
  # Change 'RR1' to 'RRK' in org_code
  mutate(org_code = as.character(org_code),
         org_code = ifelse(org_code == "RR1", "RRK", org_code)) %>% 
  # Assign org_name based on org_code
  mutate(org_name = case_when(
    org_code == 'RQ3' ~ 'BWCH',
    org_code == 'RRK' ~ 'UHB'
    
  )) %>% 
  group_by(period, org_code, org_name, type) %>% 
  summarise(
    attendances = sum(attendances),
    breaches = sum(breaches),
    admissions = sum(admissions),
    .groups = 'drop'  # Drop grouping structure after summarising
  ) %>% 
  ungroup() %>% 
  clean_names(case = "upper_camel")

# Determine the last point for each OrgName for annotations
last_points <- sub3 %>%
  arrange(OrgName, Period) %>%
  group_by(OrgName) %>%
  summarize(EndPeriod = last(Period), EndBreaches = last(Breaches))
            
# Create the plot
         
p <- plot_ly(sub3,
        type = 'scatter',
        mode = 'lines') %>% 
  add_trace(x = ~Period,
            y = ~Breaches,
            color = ~OrgName
            ) %>% 
  layout(showlegend = FALSE,
         title = 'Time Series of 4 Hour Breaches',
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector = list(
                        buttons = list(
                          list(count = 1, label = "1m", step = "month", stepmode = "backward"),
                          list(count = 6, label = "6m", step = "month", stepmode = "backward"),
                          list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
                          list(count = 1, label = "1y", step = "year", stepmode = "backward"),
                          list(step = "all")
                        )
                      ))) %>% 
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    plot_bgcolor='#e5ecf6', margin = 0.1, width = 900)

for(i in 1:nrow(last_points)) {
  p <- p %>% add_annotations(
    x = last_points$EndPeriod[i], 
    y = last_points$EndBreaches[i], 
    text = last_points$OrgName[i], 
    xref = "x", 
    yref = "y",
    showarrow = TRUE,
    arrowhead = 5
  )
}

p

# Exercise:  Now plot attendance, breach and admissions on the same plot(s).
# Your style, do it as you like, but make it look nice...

# CLUE:  pivoting is helpful here... 


