#Libraries#
library(tidyverse)
library(pdftools)
library(ggthemes)
library(lubridate)

#Global Variables
for_download <- TRUE

#Create a character vector of the links where the pdfs are
today <- Sys.Date() 

#30th of August is the first day when numbers of (un)vaccinated patients reported
first_report <- as.Date.character("2021-08-30")

#All days between first day of report and today
dates_wanted <- 
  #If it's run later than 15:00, include current day
  if(lubridate::hour(Sys.time()) > 15){
    
    seq.Date(from = first_report, to = today, by = 1)
    
  }else{seq.Date(from = first_report, to = today-1, by = 1)}

#In the form that each date appears in each link
days_wanted <- 
  dates_wanted %>% lubridate::month() %>% as.character() %>% str_pad(2,"left","0") %>%
  paste0(dates_wanted %>% lubridate::day() %>% as.character() %>% str_pad(2,"left","0"))

#for some reason, 22/09 is represented as 09222 in its link
days_wanted[days_wanted == "0922"] <- "09222" 

#Finally create links
links <- paste0("https://eody.gov.gr/wp-content/uploads/2021/",
                substr(days_wanted,1,2),
                "/covid-gr-daily-report-2021",
                days_wanted,
                ".pdf")  

#Download or load from previous runs
if(for_download){
  
  data <- sapply(links,pdftools::pdf_text)
  #save(data, file = "EOPY.RData")
  
}else{
  load("EOPY.RData")
}


#Pattern that will return us numbers
patt_vaxed <- "[\\d\\.\\(%\\)\\.]+ .* ανεμβολίαστοι"

#Locate where our pattern exists
temp <- data %>% sapply(str_locate,patt_vaxed)

#initiate a dataframe
tem_df <- data.frame()

r <- 1
for(j in 1:length(data)){
  
  
  which_page <- (!(temp[[j]][,1] %>% is.na())) %>% which()
  
  if(identical(which_page,integer(0))){
    
    next
    
  }else{
    
    tem_df[r,"File"] <- days_wanted[r]
    
    lower_border <- temp[[j]][,1][!(temp[[j]][,1] %>% is.na())]
    
    upper_border <- temp[[j]][,2][!(temp[[j]][,2] %>% is.na())]
    
    
    tem <- data[[j]][which_page] %>% 
      substr(lower_border-100, upper_border+90) %>% 
      str_extract_all("\\d{2,} \\(\\d{1,2}\\.\\d+%\\)")
    
    tem_df[r,"Unvaccinated"] <- tem[[1]][1]  
    
    tem_df[r,"Vaccinated"] <- tem[[1]][2]  
    
    r <- r+1
    
  }
  
}

#Separate absolute numbers from percentages
for(col in c("Unvaccinated", "Vaccinated")){
  
  tem_df <- 
    tem_df %>% separate( col = col, into = c(col,paste0(col,"_perc")), sep = "\\s" ) #%>% pull(col)
  
}

#Clean Percentages
for(col in paste0(c("Unvaccinated", "Vaccinated"),"_perc")){
  
  tem_df[,col] <-
    tem_df[,col] %>% str_extract("\\d+\\.\\d+") %>% as.numeric()

}

#Fix Date
tem_df[,"Date"] <-
 tem_df[,"File"] %>%
   substr(1,4) %>%
   paste0("2021") %>%
   as.Date.character(format = "%m%d%Y")


wanted_width <- 800
wanted_height <- 700

#Abolute Numbers per Day

png(filename="Absolute_Numbers_in_ICUs.png", width=wanted_width, height=wanted_height)

tem_df %>% 
  
  pivot_longer(cols = c("Unvaccinated", "Vaccinated")) %>% 
  
  mutate(value = as.integer(value)) %>% 
  
  ggplot(aes(x = Date, y = value, fill = name )) +
  
  geom_col(position = "dodge",width = .4) + 
  
  geom_label(aes(label = value, y = value),show.legend = F) + 
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  
  coord_flip() +
  
  theme_fivethirtyeight() + 
  
  labs(title = "Patients in Intensive Care Units in Greece", y = "Number of Patients", x = "") + 
  
  scale_fill_discrete(name = "",type = c("#f24b3f", "#4c92f5") ) 


dev.off()


#And one with percentages

png(filename="Percentages_in_ICUs.png", width=wanted_width, height=wanted_height)

tem_df %>% 
  
  pivot_longer(cols = paste0(c("Unvaccinated", "Vaccinated"),"_perc")) %>% 
  
  ggplot(aes(x = Date, y = value, fill = name )) +
  
  geom_col(position = "stack",width = .4) + 
  
  geom_label(aes(label = paste0(round(value),"%"), y = value/2),show.legend = F) + 
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  
  coord_flip() +
  
  theme_fivethirtyeight() + 
  
  labs(title = "Percentages of Total Patients in Intensive Care Units in Greece", y = "Percentage of Total Patients", x = "") + 
  
  scale_fill_discrete(name = "",type = c("#f24b3f", "#4c92f5") ) 


dev.off()




#Distribution
png(filename="distribution.png", width=wanted_width+50, height=wanted_height-50)

tem_df %>% 
  
  pivot_longer(cols = c("Unvaccinated", "Vaccinated")) %>% 
  
  mutate(value = as.integer(value)) %>% 
  
  ggplot(aes(x = value, fill = name )) +
  
  geom_density(alpha = 0.7) + 
  
  theme_fivethirtyeight() + 
  
  labs(title = "Distribution of Daily Number of Patients per Group", 
       x = "Daily Count of Patients", 
       y = "Frequency") + 
  
  scale_fill_discrete(name = "",type = c("#f24b3f", "#4c92f5") ) 

dev.off()

