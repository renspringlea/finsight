#!/usr/bin/env bash
notify-send -u critical "Beginning finsights"
cd ~/finsight
Rscript finsights_01_download_data.R
Rscript finsights_02_wrangle_data.R
#bundle lock --add-platform x86_64-linux
#bundle exec jekyll build --verbose
git pull
git add .
git commit -m "Update"
git push
notify-send -u critical "Completed finsights"
