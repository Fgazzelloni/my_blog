# blogdown::build_site()

# to see your site in the viewer pane
blogdown::serve_site()

# all the blog content is in the content folder/blog
# after have made some changes
# pull, commit and push to GitHub

# cmd + a (select all commits)
# cmd + shift + b (publish)

blogdown::stop_server()

## ---- COMMENTS -----
# in the config.toml file 
# baseURL = "/" need to be left like is
# RSS feeds are in "public" folder
# ????make sure ‘RSS’ is listed in our outputs within your config.toml file.
# added the link to the blog site in the config.toml

DEPLOYMENT
# once pushed all changes 
# Go to https://app.netlify.com/teams/federicagazzelloni/overview
# then: https://app.netlify.com/teams/federicagazzelloni/builds
# and choose the last deployment
# clear cache and re-deploy


