Repository for the website [christophscheuch.github.io](https://christophscheuch.github.io/) based on the [academic theme](https://github.com/gcushen/hugo-academic) for [Hugo](https://gohugo.io/) built with [blogdown](https://bookdown.org/yihui/blogdown/) in [RStudio](https://www.rstudio.com/).

## How To

Install blogdown in R using: 

```
install.packages("blogdown”) 
```

Then start a new project, entering “gcushen/hugo-academic” as the Hugo theme (keep all other options ticked). This will download all the necessary files. This website theme has been specifically designed for academics, as there are sections for info on publications, teaching, and upcoming talks, etc.

To build and view your site, run these two commands: 
```
blogdown::build_site() 
blogdown::serve_site() 
```
The viewer window will render a mobile version of your site, but you can also see a desktop version in your browser by clicking the 'Show in new window' button. Now you can see what your website will look like. All you have to do is to change the information in your scripts. 

Let’s start with basic configuration in the config.toml file. You’ll want to change the website title, name details, and organisation info. In the config file you can also change the colour theme and font style of your website, so have a play around with these options.

Your main homepage is made up of a set of widgets, which you can customise or remove entirely. For example, let’s say we want to remove the big header image, called the “hero” widget. Go to the Files tab, and then navigate to `content > home > hero.md`. Just change the “true” command after “active”, to “false”. Once you save the updated "hero" script, your website will automatically update, with the hero widget removed.

Let’s now update the profile photo. Just save your profile in the `img` folder, calling the file portrait.jpg. This will automatically update your profile picture.

The home folder contains scripts for all your home page widgets. Edit the `about` file to edit your education and biography details. Have a look through the other widgets and edit or remove. Not sure what something does? Just edit your script and see what happens!

If your website isn’t updating, this could mean that one of your files has an error. Unfortunately, blogdown sometimes won’t reveal the source of the error. You can get more info by accessing Hugo in Terminal. In RStudio, select the Terminal tab, then run the following command `hugo -v`. If there’s an error, this command will reveal which file is the cause of the problem. If there are no problems, you’ll get info about your webpage.

To edit your publications, go to the files found in the `content > publication` folder. Select the `clothing-search` file for editing. Edit your publication’s details accordingly, then save it with a useful name. It should appear in your publications.

When you are ready to deploy your site, run
```
blogdown::hugo_build()
```
and Hugo builds your site into the `public` folder. 

To add comment feature follow the instructions [here](https://mscipio.github.io/post/utterances-comment-engine/)