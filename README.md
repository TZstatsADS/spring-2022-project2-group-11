# Project 2: Visiting NYC in Covid-19

## Overview

Our team presents a vision for a travel agency that helps tourists visit New York City safely and explore during Covid times. Travel agencies are popular business where travelers, especially those from further away, have an agent with more expierence make their arrangements and plan their trip. This allows for a leaisurely trip without having to worry about planning!

We believe that these agencies should rely more on data driven models for their recommendations. Using live data from New York City, it is possible to recommend where to eat, where to be safely and how to interact with the city. We present dashboards that take a creative approach to open New York City data and put it in the eyes of a travel agent or a potentional tourist. 

The four tabs presented are a proof of concept, illustrating 
- Restaurant
	+ Restaurant Seating: A map showing the locations of restaurants in NYC with information of restaurant’s name, address, qualification of alcohol, which users can select the boroughs and type of seats they want to know. 
	+ Restaurant Inspection: A bar plot shows the distribution of the number of restaurants and the inspection grades of restaurants for each cuisine type in each borough. Users can select the borough of their interests. By clicking the checkbox ‘Comparing each borough’, users can get a plot simply comparing the the number of restaurants and the inspection grades of restaurants of boroughs.
		+ A: < 14 points (Pass Inspection)
		+ B: 14-27 points
		+ C: > 27 points 

- Events
	+ In this part we want to display distribution of events, users could filter the events according to time, location and event types. It also allows the users to choose between total space and number of events. Plan your trip according to your schedule and borough preferences!

- Safety
	+ Safety information: It includes a frequency map of arrest, shooting, and use-of-force information, with choices of any precinct in New York. It also contains a line chart of the number of changes in people's selected precinct and safety type within a year, which can be compared using the line chart of covid cases within a year below.
	+ Precinct safety comparison: It includes choices of two precincts each time to compare all three types of safety information using pie charts and line charts. It aims to provide an intuitive safety analysis for those who need to decide on travel locations.

- Precautions 

These should give an overview of this new city and help people know what to expect on their travels. 


![screenshot]<img width="1241" alt="Screen Shot 2022-02-23 at 12 38 10 PM" src="https://user-images.githubusercontent.com/57121482/155375589-f784a217-8f67-4ca8-9d17-f0483a713fdf.png">


## Project Title: NYC in Covid
Term: Spring 2022
+ Team #11
+ **Team members**: 
	+ Kaimin Wang
	+ Noah Love
	+ Kexin Tang
	+ Weixu Qian
+ **Shiny Link**: https://noahlove.shinyapps.io/nycCovid/
+ **Project summary**: 

During the covid-19 pandemic, people's lives changed dramatically, especially for people living in big cities like New York. To control the number of confirmed cases, restaurants experienced closing in the earlier stage of covid-19 and reopening in the summertime, when they could sell food for takeout or delivery. Within the popularization of covid vaccines, restaurants provide outdoor dining, including on the sidewalk or roadway.

Another aspect that influenced greatly by covid-19 is the safety issue. As the rise in unemployment and economic downturn caused by covid, unstable social events happened more frequently while people admired more and more security. The safety part is critical for people in NYC. Therefore, our travel agency app developed safety information about the arrest, shooting, and use-of-force to give users important analysis data and intuitive comparison graphs. We also produced the cause of death in New York to share traveler-related health advice.

NYC is a big international city. For people who travel to NYC, one of the most helpful things is the introduction of events. There are various museums, parks, landmarks, performances all over NYC, with different opening times. Our travel agency app An interactive event map is produced, allowing the traveler to choose the type of event of interest, the time of participation, and the geographic location.

[Contribution Statement] Noah, Kaimin, Weixun, and Kexin all contributed to the planning and ideas phase of the project. Kexin created the Restaurant tab. Weixun created the events tab. Kaimin created the safety tab and Noah created the precautions tab. Kexin initially updated the README and was later developed by Noah and Kaimin modified the content. Noah implemented the shiny design including the Homepage and About page. Each person added the description of their tab. Kaimin is the presenter. Noah packaged and uploaded the file to shiny Rstudio cloud. 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.



```
proj/
├── app/
├── lib/
├── data/
├── doc/
└── output/
```

Please see each subfolder for a README file.

