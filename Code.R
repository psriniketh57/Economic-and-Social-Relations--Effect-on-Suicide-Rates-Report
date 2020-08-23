#CODE FOR DATA CLEANING/INITIALIZING

#load libraries
library(ggplot2)
library(maps)
library(dplyr)

#load relevant datasets into Rstudio in Rstudio
mas <- read.csv("master.csv")
world_map <- map_data("world")

#rename column names
colnames(mas) <- c("country", "year", "sex", "age", "suicides_no", "population", 
                   "suicides_100k_pop", "country_year", "hdi", "gdp_year",
                   "gdp_capita", "generation")

#Add columns
region <- mas$country
mas <- cbind(mas, region)
mas$region <- as.character(mas$region)

#Data Cleaning for whole data set
  #converting all columns with numbers to numeric

  mas$year <- as.numeric(mas$year)
  mas$suicides_no <- as.numeric(mas$suicides_no)
  mas$population <- as.numeric(mas$population)
  mas$suicides_100k_pop <- as.numeric(mas$suicides_100k_pop)
  mas$hdi <- as.numeric(mas$hdi)
  mas$gdp_year <- as.numeric(mas$gdp_year)
  mas$gdp_capita <- as.numeric(mas$gdp_capita)


  #Figure 3.1
  #Replace region names that differ from the corresponding region names
  #in the world_map dataframe
  
  mas$region[which(mas$region == "United States")] <- "USA"
  mas$region[which(mas$region == "Republic of Korea")] <- "South Korea"
  mas$region[which(mas$region == "Russian Federation")] <- "Russia"
  mas$region[which(mas$region == "Saint Vincent and Grenadines")] <- "Grenadines"
  mas$region[which(mas$region == "Saint Kitts and Nevis")] <- "Saint Kitts"
  
  world_map <- left_join(world_map, mas, by = "region")
  
  #filter data by selected year
    #Year 1995 data
    world_1995 <- filter(world_map, world_map$year == 1995)
  
    #Year 2000 data
    world_2000 <- filter(world_map, world_map$year == 2000)
  
    #Year 2005 data
    world_2005 <- filter(world_map, world_map$year == 2005)
  
    #Year 2010 data
    world_2010 <- filter(world_map, world_map$year == 2010)
  

  #Figure 3.3
  #make subset of each generation
  x <- mas[mas[ , "generation"] == "Generation X", ]
  sil <- mas[mas[ , "generation"] == "Silent", ]
  gi <- mas[mas[ , "generation"] == "G.I. Generation", ]
  boom <- mas[mas[ , "generation"] == "Boomers", ]
  mil <- mas[mas[ , "generation"] == "Millenials", ]
  z <- mas[mas[ , "generation"] == "Generation Z", ]

  #find sum of suicide numbers per generation
  nx <- sum(x$suicides_no)
  nsil <- sum(sil$suicides_no)
  ngi <- sum(gi$suicides_no)
  nboom <- sum(boom$suicides_no)
  nmil <- sum(mil$suicides_no)
  nz <- sum(z$suicides_no)

  #vector of total rows of each generation
  numofgen <- c(nx, nsil, ngi, nboom, nmil, nz)
  
  #names of each generation
  names(numofgen) <- c("Gen X", "Gen Silent", "G.I.", "Boomers", "Millennials", "Gen Z")

  #Figure 3.4
    #Data cleaning for Figure 3.4.a
    #selecting united stated suicide rates from 1985 to 1990
    decade1 <- filter(mas, year == as.factor(c(1985:1990)), country == "United States")

    #Data cleaning for Figure 3.4.b
    #selecting united stated suicide rates by male and female from 1991 to 2000
    decade2 <- filter(mas, year == c(1991:2000), country == "United States")

    #Data cleaning for Figure 3.4.c
    #selecting united stated suicide rates by male and female from 2001 to 2010
    decade3 <- filter(mas, year == c(2001:2010), country == "United States")
    
    #Data cleaning for Figure 3.4.d
    #selecting united stated suicide rates by male and female from 2010 to 2016 
    decade4 <- filter(mas, year == c(2011:2016), country == "United States")

  #Figure 3.5
    #Data cleaning for 3.5
    #subsets of top 10 suicide_no countries
    rus <- mas[mas[ , "country"] == "Russian Federation", ]
    us <- mas[mas[ , "country"] == "United States", ]
    jap <- mas[mas[ , "country"] == "Japan", ]
    ukr <- mas[mas[ , "country"] == "Ukraine", ]
    kor <- mas[mas[ , "country"] == "Republic of Korea", ]
    ger <- mas[mas[ , "country"] == "Germany", ]
    fra <- mas[mas[ , "country"] == "France", ]
    bra <- mas[mas[ , "country"] == "Brazil", ]
    po <- mas[mas[ , "country"] == "Poland", ]
    uk <- mas[mas[ , "country"] == "United Kingdom", ]

    #find the sum of every row in suicides_no column 


    totrus <- sum(rus$suicides_no)
    totus <- sum(us$suicides_no)
    totjap <- sum(jap$suicides_no)
    totukr <- sum(ukr$suicides_no)
    totkor <- sum(kor$suicides_no)
    totger <- sum(ger$suicides_no)
    totfra <- sum(fra$suicides_no)
    totbra <- sum(bra$suicides_no)
    totpo <- sum(po$suicides_no)
    totuk <- sum(uk$suicides_no)

    #vector of each sum
    numofkills <- c(totrus, totus, totjap, totukr, totkor, 
                    totger, totfra, totbra, totpo, totuk)

    #name of countries
    names(numofkills) <- c("Russia", "US", "Japan", "Ukraine", "Korea", "Germany", 
                           "France", "Brazil", "Poland", "UK")


  #Figure 3.6
  #Data cleaning for Figure 3.6
  #selecting top ten countries with the highest HDI
  top_ten <- filter(mas, country == c("Norway", "Australia", "Switzerland", "Denmark", 
                                      "Netherlands", "Germany", "Ireland", "United States", "Canada", 
                                      "Singapore"))

#CODE FOR DATA ANALYSIS

  #Figure 3.1
    #Year 1995 map
    ggplot(world_1995) + 
      geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = suicides_100k_pop)) + 
      coord_quickmap() + 
      scale_fill_gradient(name = "Suicides/100K Population", 
                          low = "white", high = "#e87500") + 
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      labs(x = "", y = "", title = "Year 1995 Suicide Rate by Population") +
      theme_dark()
  
    #Year 2000 map
    ggplot(world_2000) + 
      geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = suicides_100k_pop)) + 
      coord_quickmap() + 
      scale_fill_gradient(name = "Suicides/100K Population", 
                          low = "white", high = "#e87500") + 
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      labs(x = "", y = "", title = "Year 2000 Suicide Rate by Population") +
      theme_dark()
  
    #Year 2005 map
    ggplot(world_2005) + 
      geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = suicides_100k_pop)) + 
      coord_quickmap() + 
      scale_fill_gradient(name = "Suicides/100K Population", 
                          low = "white", high = "#e87500") + 
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      labs(x = "", y = "", title = "Year 2005 Suicide Rate by Population") +
      theme_dark()
  
    #Year 2010 map
    ggplot(world_2010) + 
      geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = suicides_100k_pop)) + 
      coord_quickmap() + 
      scale_fill_gradient(name = "Suicides/100K Population", 
                          low = "white", high = "#e87500") + 
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      labs(x = "", y = "", title = "Year 2010 Suicide Rate by Population") +
      theme_dark()
  
  
  #Figure 3.2
    #GDP per Capita, faceted
    ggplot(data = mas) +
      geom_point( aes(x = gdp_capita, y = suicides_no, color = generation))  + 
      geom_smooth( aes(x = gdp_capita, y = suicides_no)) +
      facet_wrap(~generation) +
      labs(x = "GDP per Capita", y = "Suicide Count", 
           title = "GDP per Capita vs Suicide Count") + 
      scale_color_brewer(palette = "Set2") 
    
    #GDP per capita, not faceted
    ggplot(data = mas) +
      geom_point( aes(x = gdp_capita, y = suicides_no, color = generation))  + 
      geom_smooth( aes(x = gdp_capita, y = suicides_no)) +
      labs(x = "GDP per Capita", y = "Suicide Count", 
           title = "GDP per Capita vs Suicide Count") + 
      scale_color_brewer(palette = "Set2")
  
  
  #Figure 3.3
  #Data Plotting for Figure 3.3
  #bar plot of data
  
  barplot(sort(numofgen, decreasing = TRUE), main = "Suicides per Generation", xlab = "Generation", ylab = "Number of Suicides", ylim = c(0,2500000), col = c("purple3", "navy", "blue", "green4", "yellow3", "orange2"))
  
  #Figure 3.4
  #Data Plotting for Figure 3.4.a 
  #plotting the data by year on x-axis and suicide count on the y-axis and using gender as the trend
  ggplot(data = decade1, mapping = aes(x = year, y = suicides_no)) +
    geom_smooth(mapping = aes(color = sex)) + scale_y_continuous(limits = c(0,10000)) + 
    labs(x = "Year", y = "Suicide Count", title = "Suicide trends from 1986 to 1990 in US") + 
    scale_color_brewer(palette = "Set1")
  
  #Data Plotting for Figure 3.4.b
  #ploting the data by year on x-axis and suicide count on the y axis and using gender as the trend 
  ggplot(data = decade2, mapping = aes(x = year, y = suicides_no)) +
    geom_smooth(mapping = aes(color = sex)) + 
    scale_y_continuous(limits = c(0,10000)) + 
    labs(x = "Year", y = "Suicide Count", title = "Suicide trends from 1991 to 2000 in US") + 
    scale_color_brewer(palette = "Set1")
  
  #Data Plotting for Figure 3.4.c
  #ploting the data by year on the x axis and suicide count on the y-axis and using gender as the trend 
  ggplot(data = decade3, mapping = aes(x = year, y = suicides_no)) + 
    geom_smooth(mapping = aes(color = sex)) + 
    scale_y_continuous(limits = c(0,10000)) + 
    labs(x = "Year", y = "Suicide Count", title = "Suicide trends from 2001 to 2010 in US") + 
    scale_color_brewer(palette = "Set1")
  
  #Data Plotting for Figure 3.4.d
  #ploting the data by year on x-axis and suicide count on the y-axis and using gender as the trend
  ggplot(data = decade4, mapping = aes(x = year, y = suicides_no)) + 
    geom_smooth(mapping = aes(color = sex)) + 
    scale_y_continuous(limits = c(0,10000)) + labs(x = "Year", y = "Suicide Count", 
                                                   title = "Suicide trends from 2010 to 2016 in US") + 
    scale_color_brewer(palette = "Set1")
  
  #Figure 3.5
  #Data Plotting for 3.5
  #barplot of data
  barplot(sort(numofkills, decreasing = TRUE), main = "Top 10 Highest Suicides per Country",
          xlab = "Country", ylab = "Number of Suicides", 
          col = c("red3", "blue2", "white", "blue3", "yellow", "black", "blue4", "green", "red", "red4"))
  
  #Figure 3.6
  #Data Plotting for Figure 3.6.a
  # creating box plots for each county associated with their HDI
  #since some country have the same flag color, need a different color scheme
  # using various color rather than their flag color
  
  ggplot(data = top_ten, mapping = aes(country,y = hdi , fill = country)) + 
    geom_boxplot(notch = FALSE, na.rm = TRUE, notchwidth = 0.2) + 
    labs(x = "Country", y = "HDI", title = "Top 10 Countries with highest HDI")
  
  #Data Plotting for Figure 3.6.b                                                                    
  #using the top ten countries with the highest HDI
  #creating box plots for each country associate with their suicides_no
  
  ggplot(data = top_ten, mapping = aes(country,y = suicides_no , fill = country)) + 
    geom_boxplot(notch = FALSE, na.rm = TRUE, notchwidth = 0.2) +
    labs(x = "Country", y = "Suicides_per_100k", title = "Highest HDI countries vs Suicides count")+
    scale_y_continuous(limits = c(0,700))
  
  #Data Plotting for Figure 3.6.c                                                                    
  #using the top countries with the highest HDI
  #creating box plots for each country associated with their gdp_capita
  
  ggplot(data = top_ten, mapping = aes(country,y = gdp_capita , fill = country)) + 
    geom_boxplot(notch = FALSE, na.rm = TRUE, notchwidth = 0.2) +
    labs(x = "Country", y = "GDP_capita", title = "Highest HDI countries vs GDP per capita")
  
#CODE FOR ADDITIONAL DATA
  #4.1
  #Selecting Top five countries with highest HDI
  top_five <- filter(mas, country == c("Norway", "Australia", "Switzerland", "Denmark", "Netherlands"))
  
  #ploting x as year and y as HDI
  #adding trend line of countres to analyze
  ggplot (data = top_five, mapping = aes(x = year, y = hdi)) + 
    geom_smooth(mapping = aes(color = country))+ 
    labs(x = "Year", y = "HDI", title = "Countries with the Highest HDI vs year") + 
    scale_color_brewer(palette = "Set1") 
  
  #4.2
  #Gdp per year, faceted
  ggplot(data = mas) +
    geom_point( aes(x = gdp_year, y = suicides_no, color = generation))  + 
    geom_smooth( aes(x = gdp_year, y = suicides_no)) +
    facet_wrap(~generation) +
    labs(x = "GDP per Year", y = "Suicide Count", 
         title = "GDP per Year vs Suicide Count") + 
    scale_color_brewer(palette = "Set2")
  
  #GDP per year, not faceted
  ggplot(data = mas) +
    geom_point( aes(x = gdp_year, y = suicides_no, color = generation))  + 
    geom_smooth( aes(x = gdp_capita, y = suicides_no, color = generation)) +
    labs(x = "GDP per Capita", y = "Suicide Count", 
         title = "GDP per Capita vs Suicide Count") + 
    scale_color_brewer(palette = "Set2")