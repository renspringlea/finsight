Finsight by Ren Ryba (Animal Ask), 2024 ([Contact](https://animalask.org/contact)  

The code in this folder automatically produces a website ([finsight.fish](https://finsight.fish)) full of great insights about European farmed fish for use by fish welfare advocates, using publicly available data. The data is downloaded and analysed on my personal machine using R, pushed to Github, then built as a Jekyll webpage. This process is automatically repeated monthly, so the website is updated each month.  

Software/tools:  
- R (including multiple packages) for downloading, wrangling, and visualising data
- Jekyll for producing the website from markdown files
I also use, on my personal machine, Git for pushing the website to Github and cron to run all of these things automatically.

Some key files are as follows. The first two are R scripts and the latter two are bash scripts. ~ represents the home directory on my personal machine; this is relevant because these scripts gives absolute file paths.  
- ~/finsight/finsights_01_download_data.R downloads various pieces of fish-related data from public (online) sources and saves them to file.
- ~/finsight/finsights_02_wrangle_data.R performs data analysis and visualisation on this data and saves some important results to file (in various subfolders).
- ~/finsight/code_website.sh runs all R scripts and pushes everything to Github.
- ~/finsight/website.sh just pushes to Github (doesn't run the R scripts; useful for testing etc because the first R script takes a while to download all the data).  

The first R script saves files to ~/finsight/data, which is *ignored* by Git (and hence invisible to you) because there is no need to push all these large original data files to the repository.  

Biological parameters, referred to by the second R script, are stored in ~/finsight/parameters. The remainder of the repo (~/finsight/) contains all the pieces needed to automatically build the Jekyll website.

If you want to automate things so that the data is downloaded, analysed, and built automatically, put the following into your crontab (e.g. using cron on Linux):  
@monthly ~/finsight/code_website.sh  
