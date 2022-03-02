# Canada Census Visualization I
From 2016 to 2021, Canada's population as a whole [increased by 5.2%.](https://www150.statcan.gc.ca/n1/daily-quotidien/220209/dq220209a-eng.htm) This interactive vizualization allows users to view Canada 
at the census subdivision level (5,161 regions) to see which areas outpaced/were outpaced by the national growth of 5.2%. This app may be reconstructed locally in R through the use of 
```MapByProvinceShiny.R``` and the repo map/data files from StatCan.


# View the Shiny App [Here](https://dgrantstats.shinyapps.io/CanadaCensusI/)

## Sample Map
![image](https://user-images.githubusercontent.com/56042923/156312918-885ab390-6d7b-4a01-8f20-b1287cc39e28.png)

Note: Quality flags indicate the quality of the data shown in the 2016, 2021, and %-change statistics, respectively. Quality flags other than 0,0,0 indicate that data may not be reliable.

## Sources
[Map .shp files for 2021 Census Subdivisions (StatCan)](https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21)


(The map used by the app was a simplified version of the above map, used to reduce the computation time associated with rendering small islands and other intricate geographies across Canada. Because of this simplification, small or awkwardly shaped census subdivisions may not appear as they do in reality)

[Census population data (choose file for census subdivisions), 2016-2021 (StatCan)](https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/download-telecharger.cfm?Lang=E)
