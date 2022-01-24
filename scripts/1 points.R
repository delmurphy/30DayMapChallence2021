#30DayMapChallenge 2021

#Day 1: points

#Ideas:
## Ireland: 
# top visitor attractions in 2021 with points sized by visitor numbers?
### problem - covid might have screwed this up
# whiskey distilleries coloured by age?
### let's go with whiskey for now!

#First, scrape names and dates of distilleries (current) from wiki using rvest
library(rvest)
library(dplyr)

wikiwhiskey<-"https://en.wikipedia.org/wiki/Irish_whiskey#Current_distilleries"
whiskey<-read_html(wikiwhiskey)
#body_nodes<-whiskey %>% html_node("body") %>% html_children()
#body_nodes %>% html_children()

#find the relevant element using web developer tools in firefox
#(in this case, it is an unordered list <ul> made up of list items <li>;
# we want each list item as an element in a vector)
distills <- whiskey %>% 
  html_nodes(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/ul[3]") %>%
  html_elements("li") %>%
  html_text()

#now extract the name, county and est. date of each distillery
#(name should come before first ",", county between comma and "(", date after "est.' and ")")
distills_split<-strsplit(distills, "\\, | \\(est. |\\)")
#use sapply to extract names (element [[i]][1]) and years (element[[i]][3]) 
#using the subset function "[["
dnames<-sapply(distills_split, "[[", 1)
dyears<-sapply(distills_split, "[[", 3)

#get lat long of distilleries using distillery names 
#(and the geocode() function from ggmap or tidygeocoder)
#(note tidygeocoder::geocode takes addresses as dataframes only)
#ggmap::geocode('Achill Island Distillery')
#might need to register API key(See ?register_google for details):
#Warning - google requires to set up billing, use tidygeocoder instead?
# gkeyfile<-"google maps geocode API.txt"
# gkey<-readChar(gkeyfile, file.info(gkeyfile)$size)
# register_google(key = gkey, write = TRUE)
df<-data.frame(distillery = dnames, year = dyears) %>% as_tibble()
dcoords<-df %>% tidygeocoder::geocode(distillery)

cycle1<- df %>% tidygeocoder::geocode(distillery)
table(is.na(cycle1$long))
cycle2 <- cycle1  %>% filter(is.na(cycle1$lat) )%>% select(-lat, -long) %>% geocode(distillery)
table(is.na(cycle2$long))
cycle3 <- cycle2  %>% filter(is.na(cycle2$lat) )%>% select(-lat, -long) %>% geocode(distillery)
table(is.na(cycle3$long))

