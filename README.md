# sentiment_analysis_in_finance
Repository for my Bachelor thesis at the Chair of Information Systems at Humboldt University Berlin

Sentiment Analysis is the art to derive opinions or emotional content from written texts and it is used as an important tool in the field of finance. It can be used to predict the behavior of stock markets, financial distress or abnormal financial behavior. In my Bachelor's thesis, I looked deeper into this topic: What are the main topics and methods, what are use cases and where are also limits. Furthermore, I conducted an empirical analysis. I analyzed German Corporate Social Responsibility reports and parts of annual reports to investigate an association with environmental, social and governance rating scores. The aim was to assess sentiment as a risk indicator for these issues. 

The findings where handed in in July 2019 and graded with the top mark 1.0. The thesis is published here: http://dx.doi.org/10.18452/20866

In this repository, the code for the thesis is published. I used R (statistical programming language); mostly used packages are the tidyverse and the tidytext package that uses tidyverse principles. 

Several 'chunks' of code exist. 

I analyze 4 types of texts in my thesis: Letters from the CEO from CSR/ Sustainability reports and from annual reports and sustainability reports and sections from the annual reports. Abbreviated they are: 
- NB CEO (Letter from the CEO in the Sustainability Report)
- GB CEO (Letter from the CEO in the Annual Report)
- NB NB (Sustainability Report)
- GB NB (Sustainability section in the Annual Report)
All of these have one file to read the data from the companies' reports in (named xx_read_in) and where they are processed (process_xx). 

The functions for this processing are in 'functions to clean'.
All of the results are bundled in 'results_total'. This contains also most of the graphs that are used in the analysis in the paper. 

The sustainability data are added in 'eikon' which refers to the Thomson Reuters Eikon database. 

If needed, correlation tests can be seen in 'corrtests' and some graphics are created in the 'graphics' file. These contains only the graphs that are not strictly related to the analysis (visualisation of the negation handling). 
