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
table(is.na(dcoords$long))
#17 distilleries are missing lat & long coordinates (not found on OpenStreetMaps)
#For now, manually add these 
#(I know, I know... but it's a map challenge, not a webscraping/geocoding challenge!)
latlongs<-c(53.97072234051461, -9.982721444385028,
            55.2449339490717, -7.782291671783792,
            51.71101091759034, -8.51528269945433,
            53.694976594403684, -6.367924930905539,
            53.235905544639955, -9.187067870968512,
            54.64284759234052, -5.532685073191642,
            55.02237659042661, -8.268090415502513,
            52.912723260192166, -8.71579711850303,
            54.460776250377755, -5.9038969025560775,
            54.07811179643479, -6.120826860245208,
            54.25577786410766, -8.433293515536374,
            53.61187470815737, -9.443687035135506,
            53.25999120101094, -9.076146681184568,
            53.18421863926275, -6.189957381708126,
            54.39575858803034, -5.790156700705889,
            53.343797342987216, -6.285202992819454,
            54.65446228754885, -8.633257373711402)
lats<-latlongs[seq(1,length(latlongs), 2)]
longs<-latlongs[seq(2,length(latlongs), 2)]
dcoords$lat[is.na(dcoords$lat)]<-lats
dcoords$long[is.na(dcoords$long)]<-longs
#fix lat and long of Boatyard Distillery:
byard<-c(54.476871713190626, -7.8120915790274275)
dcoords$lat[dcoords$distillery=="Boatyard Distillery"]<-byard[1]
dcoords$long[dcoords$distillery=="Boatyard Distillery"]<-byard[2]

#save data
saveRDS(dcoords, "data/1.points.RDS")

#plot points
ggplot() + 
  borders("world")
