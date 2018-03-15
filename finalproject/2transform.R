#Transform file for Movies data set 


#load packages
library(tidyr)
library(ggplot2)
library(dplyr)
library(broom)
library(magrittr)
library(jsonlite)
library(lubridate)
library(scales)
library(GGally)

#set working directory 
setwd("~/Documents/StatR-502/finalproject")

#read in data 
movies <- read.csv("Data/tmdb_5000_movies.csv")

#create field for profit
movies$profit <- movies$revenue - movies$budget

#create 0 or 1 indicator if movie was profitable 
movies$profitable <- ifelse(movies$profit > 0,"Profitable","Not Profitable")
movies$profitable.ind <- ifelse(movies$profit > 0,1,0)

#convert genre to character 
movies$genres <- as.character(movies$genres)


#Parse out genre column from JSON
genredf <- movies %>% 
           filter(nchar(genres)>2) %>%
           mutate(js=lapply(genres,fromJSON))  %>%
           unnest(js)


#how many distinct genres are there per movie? 
genre_count <- dplyr::count(genredf, title)


#Visualize distribution of genres 
ggplot(data = genre_count, aes(x = n)) + 
          geom_bar(stat = "count") + 
          theme_classic() + 
          ggtitle("Movie count by number of genres")
          

#what are the most common genres? 
genre_sum <- dplyr::count(genredf, name)


#Visualize distribution of genres 
ggplot(data = genre_sum, aes(x = reorder(name,-n), y = n)) + 
  geom_bar(stat = "identity") + 
  theme_classic() +  
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  ggtitle("Movie count by number of genres")



#Clean up movies that are included         
title_dif <-  movies %>% 
              filter(as.character(title) != as.character(original_title))
        

#find which movies 
title_dif %<>% select(title, original_title, original_language)


#unique original languages 
lang <- count(title_dif, original_language)

#select which movies are english 
nrow(subset(title_dif, original_language == "en"))


#Define multiplot 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# count movies by language 
ggplot(data = movies, aes(x = original_language)) + geom_histogram(stat = "count") #english is by far dominant

p1 <- ggplot(data = movies, aes(x = original_language, y = revenue)) + geom_bar(stat = "summary", fun.y = "mean")   #what is the average revenue 
p2 <- ggplot() + geom_point(data = movies, aes(x = original_language), stat = "count", col = "blue")
multiplot(p1,p2, cols = 2)

mean_lang <-  movies %>% 
              group_by(original_language) %>% 
              summarise(mean_rv = mean(revenue)) 




#Transform release date into date (currently stored as factor)
movies$release_date %<>% as.Date(movies$release_date, format="%Y-%m-%d")
movies$release_year <- year(movies$release_date)


#plot dates 
ggplot(data = movies, aes(x = release_year)) + 
            geom_bar(stat = "count") +  
            theme_classic() + 
            ggtitle("Movie Count by Release Year")

#summarize by year 
year.sum <- dplyr::count(movies, release_year)

#how many movies came out after year 2000 
year.sum.filter <- year.sum %>% 
                   filter(release_year >= 2000) %>% 
                   filter(release_year < 2017)

sum(year.sum.filter$n)

#plot data 
ggplot(data = year.sum.filter, aes(x = release_year, y = n)) + 
  geom_bar(stat = "identity") +   
  theme_classic() + 
  ggtitle("Movie Count by Release Year")


#what are the different alues for status? 
ggplot(data = movies, aes(x = status)) + 
  geom_bar(stat = "count") +   
  theme_classic() + 
  ggtitle("Movie Count by Status")

#what's the revenue
ggplot(data = movies, aes(x = status, y = revenue)) + 
  geom_bar(stat = "summary", fun.y = "mean") +   
  theme_classic() +  
  scale_y_continuous(label = comma) + 
  ggtitle("Movie revenue by Status")

#need to filter on status = released 



#create field for time of year - special releases 
#summer movie (June, July, August)
#Holiday (November or December)

movies$summer <- ifelse(between(as.numeric(month(movies$release_date)),6,8),1,0) 
movies$holiday <- ifelse(between(as.numeric(month(movies$release_date)),11,12),1,0) 
#create column for event 
movies %<>% mutate(event =  case_when( 
                                summer == 1 ~ "summer", 
                                holiday == 1 ~ "holiday", 
                                TRUE ~ "na"))



#Creating column indicators for each genre
#what are the unique genres 
unique(genredf$name)

#what is the average revenue per genre? 
ggplot(data = genredf, aes(x = name, y = revenue)) + 
          geom_bar(stat = "summary", fun.y = "mean")

#what are the most common genres? 
ggplot(data = genredf, aes(x = name)) + 
  geom_bar(stat = "count")


#what are the TV movies? 
tv.movie <- genredf %>% 
           filter(name == "TV Movie")


#create columns for each 
genredf$action <- ifelse(genredf$name == "Action",1,0)
genredf$adventure <- ifelse(genredf$name == "Adventure",1,0)
genredf$fantasy <- ifelse(genredf$name == "Fantasy",1,0)
genredf$science.fiction <- ifelse(genredf$name == "Science Fiction",1,0)
genredf$crime <- ifelse(genredf$name == "Crime",1,0)
genredf$drama <- ifelse(genredf$name == "Drama",1,0)
genredf$thriller <- ifelse(genredf$name == "Thriller",1,0)
genredf$animation <- ifelse(genredf$name == "Animation",1,0)
genredf$family <- ifelse(genredf$name == "Family",1,0)
genredf$western <- ifelse(genredf$name == "Western",1,0)
genredf$comedy <- ifelse(genredf$name == "Comedy",1,0)
genredf$romance <- ifelse(genredf$name == "Romance",1,0)
genredf$horror <- ifelse(genredf$name == "Horror",1,0)
genredf$mystery <- ifelse(genredf$name == "Mystery",1,0)
genredf$history <- ifelse(genredf$name == "History",1,0)
genredf$war <- ifelse(genredf$name == "War",1,0)
genredf$music <- ifelse(genredf$name == "Music",1,0)
genredf$documentary <- ifelse(genredf$name == "Documentary",1,0)
genredf$foreign <- ifelse(genredf$name == "Foreign",1,0)
genredf$tv.movie <- ifelse(genredf$name == "Tv Movie",1,0)

#add aggregate values to each movie
genre.sum <- genredf %>% 
        group_by(title) %>% 
        mutate(f.action = max(action)) %>% 
        mutate(f.adventure = max(adventure)) %>% 
        mutate(f.fantasy = max(fantasy))  %>% 
        mutate(f.family = max(family))  %>% 
        mutate(f.science.fiction = max(science.fiction))  %>% 
        mutate(f.crime = max(crime))  %>% 
        mutate(f.drama = max(drama))  %>% 
        mutate(f.thriller = max(thriller))  %>% 
        mutate(f.animation = max(animation))  %>% 
        mutate(f.western = max(western))  %>% 
        mutate(f.comedy = max(comedy))  %>%
        mutate(f.romance = max(romance))  %>%
        mutate(f.horror = max(horror))  %>%
        mutate(f.mystery = max(mystery))  %>%
        mutate(f.history = max(history))  %>% 
        mutate(f.war = max(war))  %>% 
        mutate(f.music = max(music))  %>%
        mutate(f.documentary = max(documentary))  %>% 
        mutate(f.foreign = max(foreign))  %>% 
        mutate(f.tv.movie = max(tv.movie))
    

#aggregate this into title & flags for each genre    
genre.agg <- genre.sum %>% 
             dplyr::select(title, f.action, f.adventure, f.fantasy, f.science.fiction,
                   f.crime, f.drama, 
                   f.thriller, f.animation,
                   f.western, f.family,
                   f.comedy, f.romance, f.horror,
                   f.mystery, f.history, 
                   f.war, f.music, f.documentary, 
                   f.tv.movie, f.foreign) %>% 
            distinct




#create meaningful groupings of genres 
#Action/Adventure 
#Scifi/Fantasy
#Crime
#Drama
#Horror/Thriller/Mystery
#Western 
#Comedy
#Romance
#War/History 
#Music
#Documentary
#Family/Animation


#assign a single genre category 
genre.agg$genre <- 
                     case_when( 
                       genre.agg$f.drama == 1 ~ "Drama",
                       genre.agg$f.comedy == 1 ~ "Comedy",
                       genre.agg$f.action == 1 ~ "Action/Adventure", 
                       genre.agg$f.adventure == 1 ~ "Action/Adventure",
                       genre.agg$f.romance == 1 ~ "Romance",
                       genre.agg$f.crime == 1 ~ "Crime", 
                       genre.agg$f.fantasy == 1 ~ "Sci-fi/Fantasy",
                       genre.agg$f.science.fiction == 1  ~ "Sci-fi/Fantasy",
                       genre.agg$f.family == 1 ~ "Family/Animation",
                       genre.agg$f.animation == 1 ~ "Family/Animation",
                       genre.agg$f.western == 1 ~ "Western",
                       genre.agg$f.war == 1 ~ "War/History", 
                       genre.agg$f.history == 1 ~ "War/History", 
                       genre.agg$f.music == 1 ~ "Music", 
                       genre.agg$f.documentary == 1 ~ "Documentary", 
                       genre.agg$f.tv.movie == 1 ~ "Tv Movie", 
                       genre.agg$f.thriller == 1 ~ "Thriller/Mystery/Horror" ,
                       genre.agg$f.horror == 1 ~ "Thriller/Mystery/Horror" ,
                       genre.agg$f.mystery == 1 ~ "Thriller/Mystery/Horror", 
                       genre.agg$f.foreign == 1 ~ "Foreign", 
                       TRUE ~ "error"
                       )




#get rid of fields i don't want from movies data set 
movies.trim <- movies %>% 
          dplyr::select(budget, popularity, release_date, original_language, original_title,
                 revenue, runtime, status, title, profit, profitable, profitable.ind,
                 release_year, summer, holiday, event, 
                 vote_average, vote_count)



#join this to the movies df 
movies.data <- inner_join(movies.trim, genre.agg, by = "title")



#how many movies have 0 revenue? 
no.rev <- movies %>% 
          filter(revenue == 0)

nrow(no.rev)

#how many movies have 0 budget? 
no.budget <- movies %>% 
  filter(budget == 0)

nrow(no.budget)

#plot the distribution of revenue (def needs a log transform)
ggplot(data = movies, aes(x = revenue)) + 
        geom_histogram() + 
        scale_x_continuous(label = comma)

#do the same thing for budget
ggplot(data = movies, aes(x = budget)) + 
  geom_histogram() + 
  scale_x_continuous(label = comma)



options(scipen = 1000000000)
hist(movies$revenue)
hist(movies$budget)



#Filtering out the movies that were not released in english & have a different title than original title
movies.filter <- movies.data %>%  
                 filter(original_language == "en") %>%  #movies in english
                 filter(release_year >= 2000) %>%  #movies after year 2000
                 filter(release_year < 2017) %>% #movies before year 2017 (since only 1 movie in 2017) 
                 filter(as.character(original_title) == as.character(title)) %>% #original title is equal to title (removes some additional foreign films)
                 filter(revenue > 10000) %>% #there is some data quality issue (movies that should have revenue based on wiki don't in TMDB source)
                 filter(budget > 10000) %>% #remove movies with $0 budget
                 filter(status == "Released") %>% 
                 filter(f.tv.movie == 0) 


#refactor since other levels have been removed 
movies.filter$original_language <- as.factor(as.character(movies.filter$original_language))
movies.filter$status <- as.factor(as.character(movies.filter$status))
movies.filter$original_title <- as.factor(as.character(movies.filter$original_title))


#make profitable a factor 
movies.filter$profitable <- as.factor(movies.filter$profitable)

#make genre a factor 
movies.filter$genre <- as.factor(movies.filter$genre)

#make event a factor 
movies.filter$event <- as.factor(movies.filter$event)


#Select final variables in data set (can remove individual genre columns)
movies.filter %<>% dplyr::select(title, budget, revenue, profit, profitable,  profitable.ind,
                          release_date, release_year,
                          genre, runtime, vote_average, vote_count, event)



#write to csv 
write.csv(movies.filter, file = "Data/processed_movies.csv")



