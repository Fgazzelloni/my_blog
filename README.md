# My Blog

</div>
<br>
<div align="center">
 
[![Netlify Status](https://api.netlify.com/api/v1/badges/d2da763f-17f7-4fe4-9013-7b76224438ea/deploy-status)](https://app.netlify.com/sites/federicagazzelloni/deploys)

</div>

✍🏻 https://federicagazzelloni.netlify.app/

---


If you'd like to build **your own blog** please follow the instruction here: https://github.com/ivelasq/2022-01-25_building-a-blog-with-r

```r
library(blogdown)
new_site(theme = "hugo-apero/hugo-apero", 
           format = "toml",
           sample = FALSE,
           empty_dirs = TRUE)
```

### to see your site in the viewer pane

```r
blogdown::serve_site()
```

### to update your site
Once you have made your modifications in **R**, then **pull**, **commit** and **push** your changes to GitHub to see the site updating

### publishing your blog
Your site is ready, go to **netlify.com** open an account, and add a new site linking your **GitHub** blog repo.

### Your new blog is online!

---

