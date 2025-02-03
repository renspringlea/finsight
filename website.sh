#!/usr/bin/env bash
cd ~/finsight
#bundle lock --add-platform x86_64-linux
#bundle exec jekyll build --verbose
#bundle exec jekyll serve
git config pull.rebase false
git pull
git add .
git commit -m "Update"
git push
