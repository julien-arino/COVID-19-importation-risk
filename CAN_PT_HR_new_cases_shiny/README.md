If you want to run this Shiny app on a Shiny Server, you need to establish a link to the data directory in which the file `refresh_epi_data_CAN.R` saves COVID-19 data. There are several ways to do this. One of the simplest is probably to proceed as we did: save output of `refresh_epi_data_CAN.R` to a subdirectory of the default user, then symlink the data directory.

In the case of the AWS server we are using ([here](http://35.182.10.46:3838/)), for instance, the Github code lies at
```
/home/username/github/covid-19-importation-risk
```
and the data is output to
```
/home/username/DATA
```
The Shiny app is run from the default Shiny Server location, by issuing the following command 
```
sudo ln -s /home/username/github/covid-19-importation-risk/CAN_PT_HR_new_cases_shiny /srv/shiny-server/CAN_PT_HR_new_cases_shiny
```
The data directories are then linked by using
```
sudo ln -s /home/username/DATA /home/username/github/covid-19-importation-risk/CAN_PT_HR_new_cases_shiny/data
```
