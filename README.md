# Dynamic-visulization-in-phase-I-clinical-trials
# Description: Apply the popular R package ggvis to PK data set to buide up dynamic visulization in phase I clinical trials, later will use Rshiny together ,in order to make dynamic reporting according to different orginical datasets
# install related packages
install.packages("dplyr")
install.packages("ggvis")
install.packages("readxl")
install.packages("shiny")
library("shiny")
library("dplyr")
library("ggvis")
library("readxl")

 # change it according to your own data file

pop=read_excel("pk study.xlsx")   

 # using str() and glimpse() to get a big picture of your data set
 
str(pop)
glimpse(pop)     

 # using subst() to get a specific subset

pop1=subset(pop, subset= (SUBJECT=="003"& COHORT=="1"&PERIOD=="1"&ANALYTE=="9-OH-RISPERIDONE"))  

# when you click each dots on the plot, the corresponding value of Time and concentraion will show up

if (interactive()) {
    all_values <- function(x) {
         if(is.null(x)) return(NULL)
         paste0(names(x), ": ", format(x), collapse = "<br />")
     }


base=pop1 %>%
  mutate(Time1= as.numeric(TIME), conc1= as.numeric(CONCENTRATION) )%>%
 
  ggvis(~Time1, ~conc1,fillOpacity := 0.5) %>%
  layer_points( 
                shape := input_select(label = "Choose shape:",
                                      choices = c("circle", "square", "cross",
                                                  "diamond", "triangle-up", "triangle-down")),fill := "black")%>%
  layer_lines(fill := input_select(label = "Choose color:",
                                   choices = c("red", "blue", "purple",
                                               "yellow", "green "))
              )%>%
              
  add_axis("x",
           title="Time (hour)",
           values=c(0,200,400,600,800,1000,1200,1400,1600),
           subdivide=200, 
           orient="bottom")%>% 
           
  add_axis("y",
           title="Plasma Concentration (ng/ml)",
           orient="left")


     base %>% add_tooltip(all_values, "hover")
     base %>% add_tooltip(all_values, "click")}




# Graph by treatment, so you can compare the Cmax and Tmax for those two treatments


pop2=pop%>%
mutate(sub1= as.numeric(SUBJECT))

pop1=subset(pop2, subset= (COHORT=="1"&ANALYTE=="9-OH-RISPERIDONE"))

df_list <- split(pop1, as.factor(pop1$sub1))


if (interactive()) {
    all_values <- function(x) {
         if(is.null(x)) return(NULL)
         paste0(names(x), ": ", format(x), collapse = "<br />")
     }
base=df_list[[4]]%>%
  group_by(TREATMENT)%>%
  mutate(Time1= as.numeric(TIME), conc1= as.numeric(CONCENTRATION) )%>%
  
  ggvis(~Time1, ~conc1,fillOpacity := 0.5,stroke = ~TREATMENT) %>%
  layer_points(fill=~ TREATMENT, 
    shape := input_select(label = "Choose shape:",
                          choices = c("circle", "square", "cross",
                                      "diamond", "triangle-up", "triangle-down")))%>%
  layer_lines( )%>%
  add_axis("x",
           title="Time (hour)",
           values=c(0,200,400,600,800,1000,1200,1400,1600,1800),
           subdivide=200, 
           orient="bottom")%>% 
  add_axis("y",
           title="Plasma Concentration (ng/ml)",
           orient="left")%>%
add_legend(c("fill", "shape") )


      base %>% add_tooltip(all_values, "hover")
     base %>% add_tooltip(all_values, "click")}

