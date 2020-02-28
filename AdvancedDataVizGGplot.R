## If you haven't already installed the packages we'll need, do that now
install.packages("tidyverse")
install.packages("ggeffects")
install.packages("lme4")
install.packages("raster")
install.packages("cowplot")
install.packages("ggbeeswarm")
install.packages("viridis")

## Load in your packages; we'll start with tidyverse which houses "ggplot"
## We won't be setting a working directory, since all data will be downloaded
## from online or are available through the datasets we'll load in

## tidyverse has a ton of amazing packages; loading the "tidyverse" package
## allows us to easily (and in the correct order) load in all of these packages
library(tidyverse)

############ Intro to ggplot

##Just an introduction of how you would set up your ggplot

data(iris)

##Your data goes in first! Then the items for mapping in aes (aesthetics)
##Then code the geom that you want; in this case points
##Have the color change dependent on the species in the dataset and 
##increase point size
ggplot(iris,
       aes(x = Sepal.Width,
           y = Sepal.Length))+
  geom_point(aes(color = Species),
             size = 4)

############ Mapping

## We'll need to load in the "raster" "maps" "cowplot" packages for this section
library(raster); library(maps); library(cowplot)

## We'll start by making a world map
ggplot()+
  ## borders calls polygons from databases present in the "maps" package
  borders("world")+
  theme_bw()

## You made a map! Now, let's take step further and plot counties and states
## in the US
ggplot()+
  ## county borders in grey
  borders("county",
          colour = "grey")+
  ## state borders in black
  borders("state",
          colour = "black")+
  theme_bw()

## Let's take a look at just Indiana now, with county boarders
indiana <- map_data("county", "indiana")

##Get the county names
mid_range <- function(x) mean(range(x))
seats <- do.call(rbind,
                 lapply(split(indiana,
                              indiana$subregion),
                        function(d) {
                          data.frame(lat = mid_range(d$lat),
                                     long = mid_range(d$long),
                                     subregion = unique(d$subregion))
                        }))

##Plot!
ggplot(indiana,
       aes(x = long,
           y = lat)) +
  ##Indiana only counties
  geom_polygon(aes(group = group),
               fill = "white",
               colour = "grey60") +
  ##County names
  geom_text(data = seats,
            aes(label = subregion),
            size = 3)+
  theme_bw()



## Downloading the .csv file with location information directly from NEON
## We are then going to only grab the columns (1-5, 7-11) that we will use
NEON_sites = read.csv("https://www.neonscience.org/science-design/field-sites/export")
NEON_sites = NEON_sites[,c(1:5, 7:11)]

## Let's check out the column names
colnames(NEON_sites)

## Let's make a map of the NEON sites with country borders
ggplot(NEON_sites,
       aes(x = Longitude,
           y = Latitude))+
  borders("world") + 
  geom_point()+
  theme_bw()

## Tough to see the NEON sites from this far out.
## Let's make a new map of just some countries that are near the US
ggplot(NEON_sites,
       aes(x = Longitude,
           y = Latitude))+
  borders("world",
          xlim = c(-180, -50),
          ylim = c(18,75)) + 
  geom_point()+
  theme_bw()

##We'll change the shape and color of the sites to better designate them
NEON_sites$AorT = ifelse(grepl("Aquatic", NEON_sites$Site.Type),
                         "Aquatic",
                         "Terrestrial")
NEON_sites$CorR = ifelse(grepl("Core", NEON_sites$Site.Type),
                         "Core",
                         "Relocatable")

ggplot(NEON_sites,
       aes(x = Longitude,
           y = Latitude))+
  borders("world",
          xlim = c(-180, -50),
          ylim = c(18,75)) + 
  borders("state", fill = NA)+
  geom_point(aes(shape = CorR,
                 color = AorT),
             size = 2)+
  theme_bw()

NEON_sites$CRAT = paste(NEON_sites$AorT, NEON_sites$CorR)

## Looks okay, but let's make those shapes and colors better
ggplot(NEON_sites,
       aes(x = Longitude,
           y = Latitude))+
  borders("world",
          xlim = c(-180, -50),
          ylim = c(18,75)) + 
  borders("state", fill = NA)+
  geom_point(aes(shape = CRAT,
                 fill = CRAT),
             size = 3,
             alpha = 0.7)+
  ##Change the fill of the points based on order of the levels in the factor
  scale_fill_manual(values = c("#2892D7",
                               "#2892D7",
                               "#556F44",
                               "#556F44"),
                    name = "")+
  ##Change the shape of the points based on order of the levels in the factor
  scale_shape_manual(values = c(21,23,21,23),
                     name = "")+
  ##Zoom to specific x and y limits
  coord_cartesian(ylim = c(18, 72),
                  xlim = c(-170,-60))+
  theme_bw()+
  theme(legend.position = c(.25,.3))


## A useable map! You are on your way to become a master map maker!

## Next up: shapefiles!!
## Code to remake the figure from slide 16

##Download the shapefile data from NEON
download.file("https://www.neonscience.org/sites/default/files/NEONDomains_0.zip",
              ## CHANGE THE FILE DESTINATION HERE!
              destfile="C:/Users/Michael/Documents/NEONDomains_0.zip")

## Unzip the file
## CHANGE THE FILE DESTINATION HERE!
unzip("C:/Users/Michael/Documents/NEONDomains_0.zip",
      exdir = "C:/Users/Michael/Documents")

## Load in the shapefile
## CHANGE THE FILE DESTINATION HERE!
NEON_regions = shapefile("C:/Users/Michael/Documents/NEON_Domains.shp")

## Shapefiles are S4 objects, meaning that the typical $ operator to access data
## won't work here; for S4 objects, we use @ instead!
NEON_regions@data

## fortify() converts a mapping file (shapefile) to a data frame that can 
## more easily be plotted in ggplot
NEON_reg2 = fortify(NEON_regions, region = "DomainName")


## This may take a bit, so be patient!

MAIN = ggplot(NEON_sites, aes(x = Longitude, y = Latitude))+
  borders("state") + 
  geom_polygon(data = NEON_reg2,
               aes(x = long,y = lat,group = group, fill = id),color = "white")+
  borders("state", fill = NA)+
  geom_point(shape = 21, size = 2)+
  coord_cartesian(xlim = c(-65,-135),
                  ylim = c(20, 50))+
  guides(fill = guide_legend(nrow = 7, byrow = FALSE))+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank())
MAIN

HAWAII = ggplot() +
  geom_polygon(data = NEON_reg2,
               aes(x = long,y = lat,group = group, fill = id),color = "white")+
  geom_point(data = NEON_sites, aes(x = Longitude, y = Latitude),
             shape = 21, size = 2)+
  coord_cartesian(xlim = c(-161, -154), ylim = c(18, 23))+
  theme_bw()+
  theme(legend.position = "None",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

ALASKA = ggplot() +
  geom_polygon(data = NEON_reg2,
               aes(x = long,y = lat,group = group, fill = id),color = "white")+
  geom_point(data = NEON_sites, aes(x = Longitude, y = Latitude),
             shape = 21, size = 2)+
  coord_cartesian(xlim = c(-170, -130), ylim = c(50, 75))+
  theme_bw()+
  theme(legend.position = "None",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())


PUERTORICO = ggplot() +
  geom_polygon(data = NEON_reg2,
               aes(x = long,y = lat,group = group, fill = id),color = "white")+
  geom_point(data = NEON_sites, aes(x = Longitude, y = Latitude),
             shape = 21, size = 2)+
  coord_cartesian(xlim = c(-69, -64), ylim = c(17, 19))+
  theme_bw()+
  theme(legend.position = "None",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

##Get ratios for plotting "correct" proportions of Alaska, Hawaii, Puerto Rico
(ratioHawaii  <- (23 - 18) / (-154 - (-161)))
(ratioAlaska  <- (75 - 50) / (-130 - (-170)))
(ratioPR  <- (19 - 17) / (-64 - (-69)))


ggdraw(MAIN) +
  draw_plot(ALASKA, width = 0.26, height = 0.26 * 10/6 * ratioAlaska, 
            x = 0.0725, y = 0.37) +
  draw_plot(HAWAII, width = 0.15, height = 0.15 * 10/6 * ratioHawaii, 
            x = 0.325, y = 0.37) +
  draw_plot(PUERTORICO, width = 0.15, height = 0.15 * 10/6 * ratioPR, 
            x = 0.725, y = 0.37)



#####################
####Correctly plotting mixed effects models in R

##Load in ggeffects and lme4
library(ggeffects)
library(lme4)

##Load in an easy to use dataset from lme4
data(sleepstudy)

##Let's take a look at differences among ggpredict, ggeffects, and ggemmeans

##Fit a linear model
## Response ~ Predictor(s)
m2 <- lm(Reaction ~ Days * Subject, data = sleepstudy)
summary(m2)


##Same terms, same model, different functions
##The difference lies in how they treat categorical predictors when they are not
##in the model (marginal effects of your continuous variable; holding categorical
##predictors at the baseline (ggpredict) or median value (ggeffect and ggemmeans))
out.pred = ggpredict(m2, terms = c("Days"))
out.eff = ggeffect(m2, terms = c("Days"))
out.emm = ggemmeans(m2, terms = c("Days"))

colors <- c("ggpredict" = "black",
            "ggeffect" = "red",
            "ggemmeans" = "blue")


##Plot the output from these
ggplot(out.pred,
       aes(x = x,
           y = predicted))+
  geom_ribbon(aes(ymax = conf.high,
                  ymin = conf.low),
              alpha = 0.2)+ 
  geom_line(aes(color = "ggpredict"), 
            size = 1)+
  geom_ribbon(data = out.eff,
              aes(ymax = conf.high,
                  ymin = conf.low),
              alpha = 0.2,
              fill = "red")+ 
  geom_line(data = out.eff,
            aes(color = "ggeffect"), 
            size = 1)+
  geom_ribbon(data = out.emm,
              aes(ymax = conf.high,
                  ymin = conf.low),
              alpha = 0.2,
              fill = "blue")+ 
  geom_line(data = out.emm,
            aes(color = "ggemmeans"), 
            size = 1)+
  scale_color_manual(values = colors)+
  ylab("Reaction")+
  xlab("Subject")+
  theme_classic()+
  theme(legend.position = c(0.25, 0.75))

##Note how the ggeffect and ggemmeans lines are the same!
##Both are different from ggpredict though
##This is because ggeffect and ggemmeans are holding the categorical predictor
##at a constant level that is proportional to the nnumber of predictors
##ggpredict, however, holds the categorical predictor at the reference level


##Now let's take a look at mixed effects models!

##Fit a mixed effects model of Reaction time by days with random slope of
##days by subject (Days | Subject)
m1 <- lmer(Reaction ~ Days + (Days | Subject),
           data = sleepstudy)

##Take a look at the intercept and slope coefficients of the fixed (main effect)
##portion of the model; intercept is 251 seconds; and reaction time increases
##by 10.46 seconds per day
fixef(m1)


##Lets plot the main effects with the correct error estimate!
##Terms specifies the terms that are present in the fixed portion of your model
##Type specifies whether it is fixed (main) or random (mixed effects) portions
fixeff.m1 = ggeffect(m1,
                     terms = c("Days"),
                     type = "fe")

ggplot(sleepstudy,
       aes(x = Days,
           y = Reaction))+
  geom_point(shape = 21,
             size = 3)+
  geom_ribbon(data = fixeff.m1,
              aes(x = x, 
                  y = predicted,
                  ymin = conf.low,
                  ymax = conf.high), 
              alpha = 0.2)+
  geom_line(data = fixeff.m1, 
            aes(x = x,
                y = predicted), 
            color = "black", 
            size = 1)+
  theme_classic()


##Since we have a random slopes model, let's take a look at how the reaciton of
##individual subjects changes through time
##NOTE this time we are using ggpredict! Not ggeffect or ggemmeans; this is 
##because ggpredict PREDICTS the values based on the information we give it
##Here we want to know the predicted values for the subjects across days
##Terms again, we are looking at both the slope (days) and intercept (subject)
##of the model
##Type we use "re" instead to specify that we are looking at the random portions
raneff.m1 = ggpredict(m1, terms = c("Days", "Subject"), type = "re")


ggplot(raneff.m1,
       aes(x = x,
           y = predicted))+
  geom_line(aes(group = group), 
            alpha = 0.75, 
            color = "grey")+
  geom_ribbon(data = fixeff.m1,
              aes(x = x,
                  ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.2)+
  geom_line(data = fixeff.m1,
            aes(x = x,
                y = predicted),
            color = "black", 
            size = 1)+
  ylab("Reaction")+
  xlab("Days")+
  theme_classic()





##If you want to show variation across your random terms (slope or intercept),
##but don't want to have multiple lines on the same figure, you can use a 
##Random forest plot (also know as a dotplot)

##This will create a random forest plot for intercepts only!

##Create a data frame
##replace "m1" with model name
##replace "Subject" with random term is
##replace "sleepstudy" with your dataframe
dat2 = data.frame(Intercepts = ranef(m1, condVar = TRUE)$Subject[ , 1],
                  sd.interc = 1.96*sqrt(attr(ranef(m1, condVar = TRUE)[[1]],
                                             "postVar")[1, 1, 1]),
                  lev.names = unique(sleepstudy$Subject))

##This gets 95% CI points
dat2$conf.low = dat2$Intercepts - dat2$sd.interc
dat2$conf.high = dat2$Intercepts + dat2$sd.interc

##Color points based on whether it is above or below the population level estimate
dat2$direction = ifelse(dat2$Intercepts > 0, "pos", "neg")

##Reorder subject factor levels based on the values of the Intercepts data
dat2$lev.names = fct_reorder(dat2$lev.names,
                             dat2$Intercepts)

ggplot(dat2, aes(x = as.factor(lev.names),
                 y = Intercepts))+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high),
                width = 0)+
  geom_point(aes(color = direction), size = 3)+
  scale_color_manual(values = c("red", "blue"))+
  ylab("Intercept Estimate")+
  xlab("Subject")+
  coord_flip()+
  theme_classic()+
  theme(legend.position = "none")


##Here is code to look at the random slopes

##Create a data frame
##replace "m1" with model name
##replace "Subject" with random term is
##replace "sleepstudy" with your dataframe
dat3 = data.frame(Slopes = ranef(m1,
                                 condVar = TRUE)$Subject[ , 2] + fixef(m1)[2],
                  sd.slopesc = 1.96 * sqrt(attr(ranef(m1,
                                                      condVar = TRUE)[[1]],
                                                "postVar")[2, 2, 1]),
                  lev.names=as.factor(unique(sleepstudy$Subject)))

##This gets 95% CI points
dat3$conf.low = dat3$Slopes - dat3$sd.slopesc
dat3$conf.high = dat3$Slopes + dat3$sd.slopesc

##This code creates a factor that we will use to color our points
##If the confidence bands overlap 0, that subject will be coded as "none" for
##no change; if the confidence bands are below 0, that subject will be coded as
##"decrease"; if the confidence bands are above 0, that subject will be coded as
##"increase".
##"None" = slope for that level is not different from zero
##"Decrease" = slope is negative and response decreases as predictor increases
##"Increase" = slope is positive and response increases as predictor increases
dat3$direction = ifelse(dat3$conf.low < 0 & 0 < dat3$conf.high,
                        "none",
                        ifelse(dat3$conf.low < 0 & 0 > dat3$conf.high,
                               "decrease",
                               "increase"))

##Reorder subject factor levels based on the values of the slopes
dat3$lev.names = fct_reorder(dat3$lev.names,
                             dat3$Slopes)

ggplot(dat3, aes(x = lev.names, y = Slopes))+
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high),
                width = 0)+
  geom_point(aes(fill = direction), size = 3, shape = 21)+
  ##A line at zero
  geom_hline(yintercept = 0)+
  ##Another line at the population mean slope (fixed effect)
  geom_hline(yintercept = fixef(m1)[2], linetype = "dashed")+
  scale_fill_manual(values = c("red", "grey43"))+
  ylab("Slope Estimate")+
  xlab("Subject")+
  coord_flip()+
  theme_classic()+
  theme(legend.position = "none")





################################################################################
##Plot customization

library(viridis)

##Let's load in a new dataset
data(iris)
colnames(iris)

##Custom y axis, color gradient based on x axis
ggplot(iris,
       aes(x = Petal.Length,
           y = Petal.Width))+
  geom_point(aes(color = Petal.Length),
             size = 2)+
  ##Log transformed y axis
  ##Breaks at 0.1, 0.25, 0.5, 1, 2.5
  scale_y_continuous(trans = "log",
                     breaks = c(0.1, 0.25, 0.5, 1, 2.5),
                     labels = c("0.1 mm",
                                "0.25 mm",
                                "0.5 mm",
                                "1 mm",
                                "2.5 mm"))+
  ##Viridis color gradient
  scale_color_viridis()+
  ylab("Petal Width (mm)")+
  xlab("Petal Length (mm)")+
  theme_classic()



##Discrete colors, boxplots, and correctly jittered points

library(ggbeeswarm)

ggplot(iris,
       aes(x = Species,
           y = Petal.Width))+
  ##Add a boxplot! Remove outliers, since we will be plotting data with it
  geom_boxplot(outlier.shape = NA)+
  ##There is a lot of overlap in our data, so instead of using geom_point or
  ##geom_jitter, we will use geom_beeswarm to give a nice jittering of our points
  geom_beeswarm(aes(fill = Species),
                shape = 21,
                size = 4,
                alpha = 0.4)+
  ##Let's fill in our points with some nice colors
  scale_fill_manual(values = c("#817E9F",
                               "#7FC29B",
                               "#CBFF4D"))+
  ylab("Petal Width (mm)")+
  xlab("Species")+
  theme_classic()+
  ##Don't need a legend here
  theme(legend.position = "none")


##Faceting plot

ggplot(iris,
       aes(x = Petal.Length,
           y = Petal.Width))+
  ##Facet by species; label with BOTH variable and levels
  facet_wrap( ~ Species, labeller = "label_both")+
  geom_point(aes(color = Species),
             size = 4,
             alpha = 0.75)+
  scale_color_manual(values = c("#75DBCD",
                                "#805E73",
                                "#FAA381"))+
  theme(
    ##Change the size and color of the facet labels
    strip.text = element_text(size = 16),
    strip.background = element_rect(colour="black", fill="white"),
    ##No color background
    panel.background = element_rect(fill=NA),
    ##Get rid of background grid with element_blank()
    panel.grid = element_blank(),
    ##Let's use grids only on our y axis at the hashmarks
    panel.grid.major.y = element_line(color = "light grey"),
    ##No panel border
    panel.border = element_rect(color = "black", fill = NA),
    ##Axis line is just the y and x axis, not around the whole thing
    axis.line = element_line(color="black"),
    ##Legend position
    legend.position = "none",
    ##Axis text size and color
    axis.text = element_text(size=16,color="black"),
    ##Axis title size and color
    axis.title = element_text(size=18,color="black")
  )




##Custom theme, color, axis title on two lines, SAVING FIGURES

irisplot = ggplot(iris,
                  aes(x = Petal.Length,
                      y = Petal.Width))+
  geom_point(aes(color = Species),
             size = 4,
             alpha = 0.75)+
  scale_color_manual(values = c("#817E9F",
                                "#7FC29B",
                                "#CBFF4D"))+
  ##Using "\n" within your titles tells ggplot to send the next set of text on 
  ##another line
  ylab("Petal\nWidth (mm)")+
  xlab("Petal Length (mm)")+
  theme(
    ##No color background
    panel.background = element_rect(fill=NA),
    ##Get rid of background grid with element_blank()
    panel.grid = element_blank(),
    ##Let's use grids only on our y axis at the hashmarks
    panel.grid.major.y = element_line(color = "light grey"),
    ##No panel border
    panel.border = element_blank(),
    ##Axis line is just the y and x axis, not around the whole thing
    axis.line = element_line(color="black"),
    ##Legend position
    legend.position = c(0.8, 0.2),
    ##Legend title size and color
    legend.title = element_text(size=18,color="black"),
    ##Legend text size and color
    legend.text = element_text(size=16,color="black"),
    ##Get rid of the weird grey box around the legend key
    legend.key = element_rect(fill=NA,color=NA),
    ##Add a box around the legend
    legend.box.background = element_rect(color = "black"),
    legend.box.margin = margin(1, 1, 1, 1),
    ##Axis text size and color
    axis.text = element_text(size=16,color="black"),
    ##Axis title size and color
    axis.title = element_text(size=18,color="black")
  )
irisplot

ggsave("C:/Users/Michael/Documents/irisplot.tiff",
       plot = irisplot,
       dpi = 600)



