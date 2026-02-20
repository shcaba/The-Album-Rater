# The-Album-Rater
An objective way to rank albums using your subjective track ratings.
You may think you know your favorite albums within an artist or of all time.
The Album Rater will use your score to tell you objectively your highest ranked albums and artists. 
It will also show you what years you rate the highest, or have the most ratings. 

# Installing libraries

```R

Use the below code to check to make sure you have the needed libraries:

packages<-c("shiny","ggplot2","DT","bslib","readr",
"shinyWidgets","shinycssloaders","viridis","Kmedians","tidyverse","gt")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], dependencies = TRUE)
}

```

# Running the The Album Rater on the internet

The easiest way to run the app is to click on [https://shcaba.shinyapps.io/The-Album-Rater/](https://shcaba.shinyapps.io/The-Album-Rater/)


# Running the The Album Rater on your local machine

Downloading and running the tool can be accomplished in the following way:

1. Access the repository [The-Album-Rater](https://github.com/shcaba/The-Album-Rater)

 - In the green "< > Code" button, download the ZIP file.


2. Extract the folder **The-Album-Rater-master** and open the ui.r and/or server.r files in RStudio and push the "`Run App`" button 

I recommend using the "`Run External`" option within the "`Run App`" button 
(see small arrow in button to change options).

3. After hitting the "`Run App`" button, a window should open with the Shiny app running.

4. If this is the first time running the app, you should download an the example ratings file (just below the heading text).

5. If you already have your ratings file scored (see rating system below), upload it to the tool and wait for record to stop spinning.


# Rating songs
Rate albums song by song (subjective ratings) using the following rating rubric:

* 10 = Stone cold classic
* 9 = Excellent
* 8 = Super solid
* 7- Good
* 6- Fair
* 5- Moderatley listenable
* 4 - Poor
* 3 - Terrible
* 2 - Disgusting
* 1 - Complete rubbish

Provide your ratings (see the download link for the "Ex_rankings.csv"" file, or just open in from repo you have downloaded as a template) as a csv file.

Upload your song scores and have the app calculate your album rankings using the following metrics:
* Median song score (central album song rating)
* Number of songs with score of 10 (number of stone cold classics)
* Number of songs with scores of 8 and above (number of exceptional songs)
* % of songs with a score of 10
* % of songs with a score of 8 ane above

The last two metrics based on percentages adjusts for albums that have more songs (e.g., double-albums).

Rankings of each album are determined on each of the above matrics.
The overall ranking metric is the weighted average of the above 5 metric rankings with the following default weighting: 0.4,0.25,0.05,0.25,0.05
This weighting upweights stone cold classics and overall quality of the album based on percentages.

The final ranking is based on the overall weighted ranking metrics.

# Ranking outputs
* A table of each metric ranking, the ranking score and overall rankings are provided in a sortable table. You can also run a Kmedians cluster analysis (using the 5 metrics) with different colors showing groups of albums with similar scores (the number of clusters is user chosen). This gets away from pure ranking an allows for a tiered approach to evalutaing albums status. If there are major deviations from the one-to-one line, then it may show the albums are more similarly ranked than the final rankings show.
* A second table in the "Artist Rankings" tab provides a summary score for each band or artist submitted.
* A third summary of high quality (8+) songs score across years (both in number of songs and percent of songs) is provided in the "Year summary" tab.
* A fourth tab, allowing comparisons of rankings between users, is still in development.


# Additional explorations
A variety of other explorations can be made with the output:
* Taking all songs scored 10 (or >8 or whatever you choose) to make a personal greatest hits
* Explore which albums have the greatest first or second halves.
* Compare the best albums among groups and compare to other people's rankings 

