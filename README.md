This was just a fun morning exercise. Let's mix multiple images to make
a palette of their principal colors using k-means. We'll also use the
totally awesome list-columns concept to `map` each image's jpeg data
into a data frame of lists that we can `map` our function that turns the
jpeg data into a list of palette colors into a new data frame.

This more-or-less copies
<http://www.milanor.net/blog/build-color-palette-from-image-with-paletter/>
with the added twist of using multiple images before creating the
palette. I wanted to see if some cartoon show palettes using this method
matched those in the
[`ggsci`](https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html)
package. Did the authors use the algorithmic approach I will use here?
Will my approach look any better? Don't know. I decided to use "Rick and
Morty" because my kids like it. I would certainly never watch such
drivel. I'm a scientist.

For the record, the one pop culture derived palette I really like is the
\[Wes Anderson palette\]( available here:
(<https://github.com/karthik/wesanderson>) and on CRAN. These are
presumably lovingly curated and created, not like the ones created by
the stupid robots I use here.

The drawback to using KNN to create palettes from images is that it's
likely that *none* of the colors created are actually in the image. They
just represent the mathematical centers of the clusters of colors.

Load libraries.

    library(tidyverse)
    library(jpeg)
    library(scales)
    library(ggsci)

Load mulitple images. They are all Google image search thumbnails so the
size is the same. This matters since we are combining images. A larger
image would have a disproportional weight in our analysis.

I first thought that, since I am combining multiple images to get one
palette, I needed to tile the images then process. No. We just care
about the pixel color values so it really doesn't matter what position
they are in. The most efficient approach is to just chain all the RGB
values together. Duh. Still we want to do some work with the individual
images so let's label them.

    rm_list<-list()
    for (n in 1:6){
      img<-jpeg::readJPEG(paste0("img/rm",n,".jpg"))
      R<-as.vector(img[,,1])
      G<-as.vector(img[,,2])
      B<-as.vector(img[,,3])
      rm_list<-bind_rows(data_frame(img=n,R,G,B),rm_list) %>% 
        arrange(img)
    }

    ## Warning: package 'bindrcpp' was built under R version 3.4.4

    rm_list <- left_join(rm_list,
                         data_frame(
                         img = c(1, 2, 3, 4, 5, 6),
                         name = c("Schwifty","Portal","Cable",
                         "Family", "Outdoor", "Wedding")
                         ))

    ## Joining, by = "img"

Show Me What You Got
====================

I chose the images to be representative of varying but typical scenes.
Let's look at the histograms for each.

![Cable](img/rm3.jpg) Cable

![Family](img/rm4.jpg) Family

![Wedding](img/rm6.jpg) Wedding

![Outdoor](img/rm5.jpg) Outdoor

![Portal](img/rm2.jpg) Portal

![Schwifty](img/rm1.jpg) Schwifty

For fun let's do some density plots of the color values.

    #make data tidy first
    rm_tidy <- rm_list %>% gather("color","level",-img,-name)
    ggplot(rm_tidy,aes(x=level,fill=color))+
      geom_density(alpha=0.7) + 
      scale_fill_manual(values=c("blue","green","red")) + 
      theme_void()

![](README_files/figure-markdown_strict/unnamed-chunk-3-1.png)

We can see some evidence of bimodality, a preference for very bright and
very dark hues. Red is more often cranked to the max, while blue is much
more evenly distributed. Perhaps that is typical of the limited palette
of cartoons or just a function of the small number of frames I chose.

    ggplot(rm_tidy,aes(x=level,fill=color))+
      geom_density(alpha=0.7) + 
      scale_fill_manual(values=c("blue","green","red")) + 
      facet_wrap(~name)+
      theme_void()

![](README_files/figure-markdown_strict/unnamed-chunk-4-1.png)

It's interesting to compare "Cable" with "Family." Both images share the
same backdrop but "Family" is much darker.

Make the palettes
=================

When I was a kid with watercolors I wanted to come up with a name for
the filthy color that resulted when I mixed all the colors together. I
called it "Hitler" (but, really, brown). What is the color that results
when we average all the RGB values? What named R colors resemble it? It
looks to me like it's between cornsilk4 and darkkhaki.

    blend_color<-rm_list %>% 
      summarise(R=mean(R),G=mean(G),B=mean(B)) %>% 
      rgb()

    show_col(c("cornsilk4",blend_color,"darkkhaki"))

![](README_files/figure-markdown_strict/unnamed-chunk-5-1.png)

Let's call it "desertkhaki" which, hopefully, is not a trigger word.

Now, for the fun part. In the Wes Anderson palette set, each movie get's
a different palette. Let's make palettes for each of the images, which I
chose for their distinctiveness.

For me, the good thing about open source is that I can stand on the
shoulders of giants in the community. R also makes very muscular
analysis trivally simple. On the other hand it makes "script kiddies"
like me potentially dangerous. I can only describe k-means in the most
general terms but can run it in a snap.

    num_colors = 16
    pal_schwifty <- rm_list %>% 
      filter(name=="Schwifty") %>% 
      select(R,G,B) %>% 
      kmeans(centers = num_colors, iter.max = 30) %>% 
      .$centers %>% 
      rgb()

    show_col(pal_schwifty)

![](README_files/figure-markdown_strict/unnamed-chunk-6-1.png)

For data plotting the separation between some of these colors is too
small. I think 9 colors will suffice.

    num_colors = 9
    pal_schwifty <- rm_list %>% 
      filter(name=="Schwifty") %>% 
      select(R,G,B) %>% 
      kmeans(centers = num_colors, iter.max = 30) %>% 
      .$centers %>% 
      as.tibble() %>% 
      {.}

    show_col(rgb(pal_schwifty))

![](README_files/figure-markdown_strict/unnamed-chunk-7-1.png)

For plotting purposes I would like use these colors in order of
intensity. Sorting colors is a [topic in
itself](http://www.alanzucconi.com/2015/09/30/colour-sorting/) but here
we'll do it quick and simple.

    pal_schwifty %>% 
      mutate(saturation=rowSums(.[1:3])) %>% 
      arrange(saturation) %>% 
      rgb() %>% 
      show_col()

![](README_files/figure-markdown_strict/unnamed-chunk-8-1.png) That's
about right. Let's put it all together. Go through all the images to
create a series of palettes.

    #function to turn a table of RGB values to an ordered list of colors
    gen_pal <- function(rgb_table) {
      num_colors = 9
      pal <- rgb_table %>%
      select(R, G, B) %>%
      kmeans(centers = num_colors, iter.max = 30) %>%
      .$centers %>%
      as.tibble() %>%
      mutate(saturation = rowSums(.[1:3])) %>%
      arrange(saturation) %>%
      rgb()
      return(pal)
    }

    #now make list columns, which are totally awesome, for each palette
    palette_rick<-rm_list %>% 
      group_by(name) %>% 
      select(-img) %>% 
      nest(.key="rgb") %>% 
      transmute(name=name,pal= map(rgb,gen_pal))
    palette_rick

    ## # A tibble: 6 x 2
    ##   name     pal      
    ##   <chr>    <list>   
    ## 1 Schwifty <chr [9]>
    ## 2 Portal   <chr [9]>
    ## 3 Cable    <chr [9]>
    ## 4 Family   <chr [9]>
    ## 5 Outdoor  <chr [9]>
    ## 6 Wedding  <chr [9]>

    #a function to extract the individual palettes, given a name.

    extract_pal<-function(palette_list,pal_name){
      pal<-palette_list %>% filter(name==pal_name) %>% 
        select(pal) %>% 
        unlist() %>% 
        as.vector()
      return(pal)
    }

    show_pal<- function(img_name,palette_rick){
      
    palette_rick %>% 
        filter(name==img_name) %>% 
        select(pal) %>% 
        unlist() %>% 
        my_show_col(label = img_name,labels = F)  
    # BTW, If you hate verbose pipes, here is the non-dplyr way of getting the same thing.
    #by_name[by_name$name=="Wedding",]$pal[[1]] %>% show_col()
    }

![Cable](img/rm3.jpg)

    show_pal("Cable",palette_rick)

![](README_files/figure-markdown_strict/unnamed-chunk-13-1.png)

![Schwifty](img/rm1.jpg)
![](README_files/figure-markdown_strict/unnamed-chunk-14-1.png)

![Portal](img/rm2.jpg)
![](README_files/figure-markdown_strict/unnamed-chunk-15-1.png)
![Family](img/rm4.jpg)
![](README_files/figure-markdown_strict/unnamed-chunk-16-1.png)
![Outdoor](img/rm5.jpg)
![](README_files/figure-markdown_strict/unnamed-chunk-17-1.png)
![Wedding](img/rm6.jpg)
![](README_files/figure-markdown_strict/unnamed-chunk-18-1.png) Finally,
let's do what we said we'd do at the beginning, put all these images
together and add it to our list column of palettes.

    multi_img_pal <- gen_pal(rm_list)
    palette_rick<-data_frame(name="all",pal=list(multi_img_pal)) %>% bind_rows(palette_rick)
    show_col(multi_img_pal)

![](README_files/figure-markdown_strict/unnamed-chunk-19-1.png)

Not too bad. I'm glad something resembling Rick's hair makes it into the
list. Compare it to the ggsci package Rick and Morty palette. Here we
see the weaknesses of an algorithmic approach. ggsci is more interesting
since it has more color diversity and vividness. I assume they were hand
selected. You can see Rick's hair and Morty's shirt color.

    show_col(ggsci::pal_rickandmorty()(9))

![](README_files/figure-markdown_strict/unnamed-chunk-20-1.png)

Make some plots.

    #use the example in help for dplyr::gather
    stocks <- data.frame(
      time = as.Date('2009-01-01') + 0:9,
      W = rnorm(10, 0, 1),
      X = rnorm(10, 0, 1),
      Y = rnorm(10, 0, 2),
      Z = rnorm(10, 0, 4)
    )
    stocksm <- stocks %>% gather(stock, price, -time)

    ggplot(stocksm,aes(time,price,color=stock))+geom_line(size=2)+
      scale_color_manual(values = multi_img_pal) + theme_minimal()

![](README_files/figure-markdown_strict/unnamed-chunk-21-1.png)

    ggplot(stocksm,aes(time,price,color=stock))+geom_line(size=2) +
      theme_minimal() +
      scale_color_manual(values = extract_pal(palette_rick,"Wedding"))

![](README_files/figure-markdown_strict/unnamed-chunk-22-1.png)
