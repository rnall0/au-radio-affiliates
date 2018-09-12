# au-radio-affiliates

The purpose of this Shiny application is to visualize Auburn University sports radio affiliate locations. This is particularly valuable for one looking to find a radio station carrying an Auburn University athletic event if they are traveling and/or unfamiliar with local radio stations.

[https://rnall.shinyapps.io/au-radio-affiliates](https://rnall.shinyapps.io/au-radio-affiliates/)

##Data
The official [Auburn Sports Network - Listen/Affiliates](https://auburntigers.com/sports/2018/6/15/auburn-sports-network-listen.aspx) page was used as the "authoritative" source to scrape call sign tower lat/lons from [radio-locator.com](https://radio-locator.com). The [radio-locator.com](https://radio-locator.com) page has daily request limits, so be aware. 

## How to use
Each location on the map contains information about what city the tower is in, call letters, and frequency, while also stating if that affiliate broadcasts an Auburn Sports Today segement, the Tiger Talk radio show, football games, mens basketball games, and/or baseball games. Click on a location to see this information in the html pop-up. The layer panel is also collapsed in the top left, and layers distinguishing those between AM and FM frequencies can be turned on and off. 

## Notes
There are some discrepancies between call signs, locations, and cities from the various sources. For example, the official Auburn Sports Network listing shows WSBM	1340 AM in Florence, AL as the only affiliate in the area, but this is in fact duplicated with W250BY 97.9 FM in Florence. Because I know this, I corrected it. 

I could not find information on WHAL in Columbus, GA. 

And others.

## Sources


- [Auburn Sports Network - Listen/Affiliates](https://auburntigers.com/sports/2018/6/15/auburn-sports-network-listen.aspx)
-  [radio-locator.com](https://radio-locator.com)
