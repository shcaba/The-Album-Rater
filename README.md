# The-Album-Rater
An objective way to rank albums using your subjective track ratings.

Rate Albums song by song (subjective ratings) using the following rating rubric:
10 = Stone cold classic
9 = Excellent
8= Super solid
7- Good
6- Fair
5- Moderatley listenable
4 - poor
3 - terrible
2 - disgusting
1 - complete rubbish

Provide your ratings use the example csv file.
Upload your song scores and have the app calcualte your album rankings using the following metrics:
* Median song score (central album song rating)
* Number of songs with score of 10 (number of stone cold classics)
* Number of songs with scores of 8 and above (number of exceptional songs)
* % of songs with a score of 10
* % of songs with a score of 8 ane above

The last two metrics based on percentages adjusts for albums that have more songs (e.g., double-albums).

Rankings of each album are determined on each of the above matrics.
The overall ranking metric is the weighted average of the above 5 metric rankings with the following default weighting: 0.4,0.1,0.1,0.2,0.2
This weighting upweights stone cold classics and overall quality of the album based on percentages.

The final ranking is based on the overall ranking metrics.

# Ranking outputs
A table of each metric ranking, the ranking score and overall rankings are provided in a sortable table.
A comparison plot is also provided comparing the ovearll ranking with the ranking metrics. If this follows the one-to-one line, then there is a clear differentitaion among the compared albums.
If there are major deviations from the one-to-one line, then it may show the albums are more similarly ranked than the final rankings show.
A cluster analysis is performed using the Kmedians package to look for clustering of ranks among the 5 metrics. Point colors show the distinct clusters found via the k-medians cluster appraoch. 

# Additional explorations
A variety of other explorations can be made with the output:
* Taking all songs scored 10 (or >8 or whatever you choose) to make a personal greatest hits
* Explore which albums have the greatest first or second halves.
* Compare the best albums among groups and compare to other people's rankings 

