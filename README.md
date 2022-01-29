# my_blog

If you'd like to build your own blog pleas follow the instruction here: https://github.com/ivelasq/2022-01-25_building-a-blog-with-r

library(blogdown)
new_site(theme = "hugo-apero/hugo-apero", 
           format = "toml",
           sample = FALSE,
           empty_dirs = TRUE)