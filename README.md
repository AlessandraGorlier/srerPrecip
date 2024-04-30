**Data Explanation:**

1)  Rain gauge data is received and updated monthly from the Santa Rita Precipitation Website (located here: https://santarita.arizona.edu/).
2)  Column format is manipulated for ease of visualization creation.
3)  -9999 values are replaced in the data set with interpolated data. Inverse Distance Weighting (IDW) interpolation was utilized for the Santa Rita Experimental Range (SRER), with the SRER boundary as the extent. Interpolation was performed and developed by Haiyan Wei ([haiyan\@arizona.edu](mailto:haiyan@arizona.edu){.email}).
4)  Long-term monthly averages were calculated for April, May, and June of 1955 and January of 1959 as replacement interpolation failed due to lack of full-data.
5)  Completed manipulated, estimated, and interpolated data is utilized to create the visualizations on the web application.

**Additional Resources and References** Standardized Precipitation Index (SPI) Explorer (University of Arizona), which allows exploring SPI values at specific locations by using a gridded climate dataset (PRISM Climate) to estimate local precipitation time series.

The SPI Explorer tool (developed by Mike Crimmins) was used as a reference for this web application.

<https://uaclimateextension.shinyapps.io/SPItool/>
