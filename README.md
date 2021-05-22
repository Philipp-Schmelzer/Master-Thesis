# Master-Thesis

This is the documentation of the R code and the code of the Shiny R implementation of the master thesis by Philipp Schmelzer (philipp.schmelzer@student.unisg.ch).

A total of three files can be found in the master folder. The data set used is stored in "bakery_sales.csv". The programmed R code for the random forest is stored in "time_series_forecasting_final.R". The "Shiny" folder contains the files for the implementation of the Shiny R website. The files "global.R", "server.R" and "ui.R" are the code parts necessary for the implementation. The files "intro.csv" and "introtext" are the text modules that are displayed in the landing page and the intro tour through the web page. The icons used in the visualisation are stored in the "www" folder. The plots of the random forest algorithm are also stored here. The results of the random forest algorithm are stored in the folder "prediction_output".

Procedure for starting the app.
1. Download all the files to your own computer.
2. Unpack the ZIP File "Master-Thesis-main"
3. Set the local working directory to the folder "Shiny_App" within the Master-Thesis-main folder .
4. Install and initialise the necessary R packages for both the R-Code and the Shiny R implementation.
5. Execute the R code.
6. Start the application in either the "global.R", "server.R" or "ui.R" file.

The web application is optimised for full screen use on a 13 inch screen. On smaller screens, there may be some displacement of the user interface.
The website is already online and can be accessed by using this link: https://philipp-schmelzer-hsg.shinyapps.io/shiny/.
