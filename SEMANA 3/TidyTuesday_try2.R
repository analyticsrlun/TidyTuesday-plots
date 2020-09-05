library(plotly)
library(tidyverse)
library(htmlwidgets)

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

key_crop_yields <- key_crop_yields %>% 
  filter(Entity =="Colombia")%>%na.omit()

names(key_crop_yields)[4]<-"Wheat"
names(key_crop_yields)[5]<-"Rice"
names(key_crop_yields)[8]<-"Potatoes"
names(key_crop_yields)[11]<-"Cassava"
names(key_crop_yields)[14]<-"Bananas"

key_crop_yields <- key_crop_yields %>% accumulate_by(~Year)

#### GRaficar

fig <- plot_ly(data = key_crop_yields, x= ~Year, y= ~Wheat, type = "scatter", 
               mode = "lines+markers", name= "Trigo", 
               line= list(color = "rgb(78, 211, 233)"),
               marker=list(color = "rgb(78, 211, 233)"),frame= ~frame)

fig <- fig %>% add_trace(x= ~Year, y= ~Rice, type = "scatter", 
                  mode = "lines+markers", name= "Arroz", 
                  line= list(color = "rgb(255, 20, 147)"),
                  marker=list(color = "rgb(255, 20, 147)"),frame= ~frame) %>% 
  
  add_trace(x= ~Year, y= ~Cassava, type = "scatter", 
                  mode = "lines+markers", name= "Yuca", 
                  line= list(color = "rgb(137, 207, 91)"),
                  marker=list(color = "rgb(137, 207, 91)"),frame= ~frame) %>%
  
  add_trace(x= ~Year, y= ~Potatoes, type = "scatter", 
            mode = "lines+markers", name= "Papa", 
            line= list(color = "rgb(150, 147, 113)"),
            marker=list(color = "rgb(150, 147, 113)"),frame= ~frame) %>%
  
  add_trace(x= ~Year, y= ~Bananas, type = "scatter", 
            mode = "lines+markers", name= "Banana", 
            line= list(color = "rgb(239, 232, 126)"),
            marker=list(color = "rgb(239, 232, 126)"),frame= ~frame)

#### Detalles de letra

xaxis <- list(title = "Año",
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2, size=6)

yaxis <- list(title = "Toneladas por hectárea",
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2, size=6)


l <- list(x=0.8, y = 1, title=list(text="<b> Cosecha </b>"),
          font = list(
            family = "sans-serif",
            size = 12,
            color = "#000"),
          bgcolor = "#E2E2E2",
          bordercolor = "#FFFFFF",
          borderwidth = 2)

#### Agregar imagenes

fig <- fig %>% layout(paper_bgcolor= "rgb(240, 240, 240)",title ="<b>Top 5 cosechas con más toneladas por hectárea en Colombia</b>",
                      xaxis=xaxis, yaxis=yaxis, legend = l,
  
     images = list(source = "https://image.flaticon.com/icons/png/512/24/24331.png",
     x = 0.28, y = 0, 
     sizex = 1, sizey = 1,
     xref = "paper", yref = "paper", 
     xanchor = "left", yanchor = "bottom",
     opacity= 1, layer="below"

  )
)

#### Agregar detalles de animacion

fig <- fig %>% animation_opts(
  frame = 200, 
  transition = 0, 
  redraw = FALSE)

fig <- fig %>% animation_slider(
  hide = T)

fig <- fig %>% animation_button(
  x = 0.1, xanchor = "right", y = 0.9, yanchor = "bottom")

fig


