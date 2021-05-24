# Master-Thesis

This is the repository dedicated to the master thesis by Philipp Schmelzer (philipp.schmelzer@student.unisg.ch).

The documentation of the expert and evaluation interviews can be found in the "Interview" folder. 
The documentation of the design workshop is available in the folder "Design Workshop".
The whole master thesis as a PDF file can be found in the folder "Thesis".

A total of three files can be found in the Shiny_App folder. It documents the R code and the code of the Shiny R implementation as well as the employed data set of the master thesis. 

The data set used is stored in "bakery_sales.csv". The programmed R code for the random forest is stored in "time_series_forecasting_final.R". The "Shiny" folder contains the files for the implementation of the Shiny R website. The files "global.R", "server.R" and "ui.R" are the code parts necessary for the implementation. The files "intro.csv" and "introtext" are the text modules that are displayed in the landing page and the intro tour through the web page. The icons used in the visualisation are stored in the "www" folder. The plots of the random forest algorithm are also stored here. The results of the random forest algorithm are stored in the folder "prediction_output".

Procedure for starting the app.
1. Download all the files to your own computer.
2. Unpack the ZIP File "Master-Thesis-main"
3. Set the local working directory to the folder "Shiny_App" within the Master-Thesis-main folder .
4. Install and initialise the necessary R packages for both the R-Code and the Shiny R implementation.
5. Execute the R code. After running the code, there should be a new folder "prediction_output" with the various outputs in        .csv files. The folder "www" should now contain the plots of the prediction results.
6. Start the application in either the "global.R", "server.R" or "ui.R" file.

The web application is optimised for full screen use on a 13 inch screen. On smaller screens, there may be some displacement of the user interface.
The website is already online and can be accessed by using this link: https://philipp-schmelzer-hsg.shinyapps.io/shiny/.
