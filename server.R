library(readr)
library(tidyverse)
library(png)
library(DT)


setwd(paste0(getwd(), "/www"))
stats <- read_csv("stats.csv")


server <- function(input, output, session){

  ##RENDER TABLE REACTIVE ON INPUTS
  observeEvent(c(input$PPG,
                 input$RPG,
                 input$APG,
                 input$Three,
                 input$SoS,
                 input$OEff,
                 input$DEff,
                 input$EffM,
                 input$TEff,
                 input$WinPer,
                 input$eFG,
                 input$TOV,
                 input$TRB,
                 input$FT,
                 input$Rand),{
                   
  stats <- stats %>% mutate(Weight = (input$PPG/10)*stats$ppgR + input$RPG/10 * stats$rpgR + input$APG/10 * stats$apgR +
                              input$Three/10 * stats$`3R` + input$SoS/10 * stats$SoSR + input$OEff/10 * stats$Orank + 
                              input$DEff/10 * stats$Drank + input$EffM/10 * stats$EM + input$TEff/10 * stats$Tempo +
                              input$WinPer/10 * stats$WinPerc + input$eFG/10 * stats$eFGR + input$TOV/10 * stats$TOVR +
                              input$TRB/10 *stats$TRBR + input$FT/10 *stats$FTR)
  
 #Randomly select percentage of rows and multiply them by a random val between 1,2
 #set seed for reproduceability
  set.seed(1)
  y <- sample(1:nrow(stats), ceiling(input$Rand/10 * nrow(stats)), replace = T)
  set.seed(1)
  stats[y, ]$Weight <- round(stats[y,]$Weight * runif(length(y), 1, 2),1)
  
  j<-16
  k<-16
  m<- 9
  stats2 <- data.frame(Seed= numeric(8), Team=character(8), Weight=numeric(8), stringsAsFactors = F)
  for(i in 1:16){
    if(i==m) break
    stats2$Seed[i] <- ifelse(stats[i,34]$Weight > stats[j,34]$Weight, stats$Seed[i], stats$Seed[j])
    stats2$Team[i] <- ifelse(stats[i,34]$Weight > stats[j,34]$Weight, stats$Team[i], stats$Team[j])
    stats2$Weight[i] <- ifelse(stats[i,34]$Weight > stats[j,34]$Weight, stats$Weight[i], stats$Weight[j])
    j=j-1
    
  }
  stats2 <<- stats2
  
  stats3 <- data.frame(Seed= numeric(8), Team=character(8), Weight=numeric(8), stringsAsFactors = F)
  j <- 1
  for(i in seq(1,8,2)){
    stats3$Seed[j] <- ifelse(stats2$Weight[i] > stats2$Weight[i+1], stats2$Seed[i], stats2$Seed[i+1])
    stats3$Team[j] <- ifelse(stats2$Weight[i] > stats2$Weight[i+1], stats2$Team[i], stats2$Team[i+1])
    stats3$Weight[j] <- ifelse(stats2$Weight[i] > stats2$Weight[i+1], stats2$Weight[i], stats2$Weight[i+1])
    j <- j+1
  }
  
  stats3 <<- stats3
  
  stats4 <- data.frame(Seed= numeric(4), Team=character(4), Weight=numeric(4), stringsAsFactors = F)
  j <- 1
  for(i in seq(1,4,2)){
    stats4$Seed[j] <- ifelse(stats3$Weight[i] > stats3$Weight[i+1], stats3$Seed[i], stats3$Seed[i+1])
    stats4$Team[j] <- ifelse(stats3$Weight[i] > stats3$Weight[i+1], stats3$Team[i], stats3$Team[i+1])
    stats4$Weight[j] <- ifelse(stats3$Weight[i] > stats3$Weight[i+1], stats3$Weight[i], stats3$Weight[i+1])
    j <- j+1
  }
  
  stats4 <<- stats4
  
  
  output$table <- renderDT(datatable(stats %>% 
                           select(-c(WinPerc, EM, Orank,Drank,Tempo,ppgR, rpgR,apgR,`3R`,SoSR, eFGR, TOVR, TRBR, FTR, img)) %>% 
                           arrange(desc(Weight)),
                           extensions='ColReorder', options=list(colReorder=T, scrollY=600, dom = 'ft'), rownames = F))
  })
  
  ##RENDER BRACKET, TEXT, & IMAGES
  output$bracket <- renderPlot({
  x<-seq(0,220,(221/67))
  y<-0:66
  plot(x,y,type="l", col.axis="white", col.lab="white", bty="n",
       axes=F, col="white")
  
  segments(0,c(seq(0,30,2),seq(34,64,2)),20,c(seq(0,30,2),seq(34,64,2)))
  segments(20,c(seq(0,28,4),seq(34,62,4)),20,c(seq(2,30,4),seq(36,64,4)))
  segments(20,c(seq(1,29,4),seq(35,63,4)),40,c(seq(1,29,4),seq(35,63,4)))
  segments(40,c(seq(1,25,8),seq(35,59,8)),40,c(seq(5,29,8),seq(39,63,8)))
  segments(40,c(3,11,19,27,37,45,53,61),60,c(3,11,19,27,37,45,53,61))
  segments(60,c(3,19,37,53),60,c(11,27,45,61))
  segments(60,c(7,23,41,57),80,c(7,23,41,57))
  segments(80,c(7,41),80,c(23,57))
  segments(80,c(15,49),100,c(15,49))
  
  segments(100,c(27,37),120,c(27,37))
  segments(200,c(seq(0,30,2),seq(34,64,2)),220,c(seq(0,30,2),seq(34,64,2)))
  segments(200,c(seq(0,28,4),seq(34,62,4)),200,c(seq(2,30,4),seq(36,64,4)))
  segments(180,c(seq(1,29,4),seq(35,63,4)),200,c(seq(1,29,4),seq(35,63,4)))
  segments(180,c(seq(1,25,8),seq(35,59,8)),180,c(seq(5,29,8),seq(39,63,8)))
  segments(160,c(3,11,19,27,37,45,53,61),180,c(3,11,19,27,37,45,53,61))
  segments(160,c(3,19,37,53),160,c(11,27,45,61))
  segments(140,c(7,23,41,57),160,c(7,23,41,57))
  segments(140,c(7,41),140,c(23,57))
  segments(120,c(15,49),140,c(15,49))
  
  size <- 8.5
  height <- 2
  start <- 65
  j<-16
  k<-16
  m<- 9
  
  for(i in 1:16){
    if(i==m) break
    text(size, start, str_pad(stats[i,1], width=30, side="right"), cex=.75, font=2)
    text(size,start,str_pad(stats[i,2], width=10, side="both"),cex=.75, font=2)
    start<- start - height
    text(size, start, str_pad(stats[j,1], width=30, side="right"), cex=.75, font=2)
    text(size,start,str_pad(stats[j,2], width=10, side="both"),cex=.75, font=2)
    start<- start - height
    j=j-1
    
  }
  

  
  
  # text(8.5,65,paste0(stats[1,1], " ", stats[1,2]),cex=.8, font=2)
  # text(8.5,63,paste0(stats[16,1], " ", stats[16,2]),cex=.8, font=2)
  # text(8.5,61,paste0(stats[8,1], " ", stats[8,2]),cex=.8, font=2)
  # text(8.5,59,paste0(stats[9,1], " ", stats[9,2]),cex=.8, font=2)
  # text(9.8,60.5,subw1[2,13],cex=.4)
  # 
  # text(9.8,58.5,subw1[2,14],cex=.4)
  # 
  # text(9.8,56.5,subw1[3,13],cex=.4)
  # 
  # text(9.8,54.5,subw1[3,14],cex=.4)
  # 
  # text(9.8,52.5,subw1[4,13],cex=.4)
  # 
  # text(9.8,50.5,subw1[4,14],cex=.4)
  # 
  # text(9.8,48.5,subw1[5,13],cex=.4)
  # 
  # text(9.8,46.5,subw1[5,14],cex=.4)
  # 
  # text(9.8,44.5,subw1[6,13],cex=.4)
  # 
  # text(9.8,42.5,subw1[6,14],cex=.4)
  # 
  # text(9.8,40.5,subw1[7,13],cex=.4)
  # 
  # text(9.8,38.5,subw1[7,14],cex=.4)
  # 
  # text(9.8,36.5,subw1[8,13],cex=.4)
  # 
  # text(9.8,34.5,subw1[8,14],cex=.4)
  # 
  # 
  # text(9.8,30.5,suby1[1,13],cex=.4)
  # 
  # text(9.8,28.5,suby1[1,14],cex=.4)
  # 
  # text(9.8,26.5,suby1[2,13],cex=.4)
  # 
  # text(9.8,24.5,suby1[2,14],cex=.4)
  # 
  # text(9.8,22.5,suby1[3,13],cex=.4)
  # 
  # text(9.8,20.5,suby1[3,14],cex=.4)
  # 
  # text(9.8,18.5,suby1[4,13],cex=.4)
  # 
  # text(9.8,16.5,suby1[4,14],cex=.4)
  # 
  # text(9.8,14.5,suby1[5,13],cex=.4)
  # 
  # text(9.8,12.5,suby1[5,14],cex=.4)
  # 
  # text(9.8,10.5,suby1[6,13],cex=.4)
  # 
  # text(9.8,8.5,suby1[6,14],cex=.4)
  # 
  # text(9.8,6.5,suby1[7,13],cex=.4)
  # 
  # text(9.8,4.5,suby1[7,14],cex=.4)
  # 
  # text(9.8,2.5,suby1[8,13],cex=.4)
  # 
  # text(9.8,0.5,suby1[8,14],cex=.4)
  # 
  # text(209.8,64.5,subx1[1,13],cex=.4)
  # 
  # text(209.8,62.5,subx1[1,14],cex=.4)
  # 
  # text(209.8,60.5,subx1[2,13],cex=.4)
  # 
  # text(209.8,58.5,subx1[2,14],cex=.4)
  # 
  # text(209.8,56.5,subx1[3,13],cex=.4)
  # 
  # text(209.8,54.5,subx1[3,14],cex=.4)
  # 
  # text(209.8,52.5,subx1[4,13],cex=.4)
  # 
  # text(209.8,50.5,subx1[4,14],cex=.4)
  # 
  # text(209.8,48.5,subx1[5,13],cex=.4)
  # 
  # text(209.8,46.5,subx1[5,14],cex=.4)
  # 
  # text(209.8,44.5,subx1[6,13],cex=.4)
  # 
  # text(209.8,42.5,subx1[6,14],cex=.4)
  # 
  # text(209.8,40.5,subx1[7,13],cex=.4)
  # 
  # text(209.8,38.5,subx1[7,14],cex=.4)
  # 
  # text(209.8,36.5,subx1[8,13],cex=.4)
  # 
  # text(209.8,34.5,subx1[8,14],cex=.4)
  # 
  # 
  # text(209.8,30.5,subz1[1,13],cex=.4)
  # 
  # text(209.8,28.5,subz1[1,14],cex=.4)
  # 
  # text(209.8,26.5,subz1[2,13],cex=.4)
  # 
  # text(209.8,24.5,subz1[2,14],cex=.4)
  # 
  # text(209.8,22.5,subz1[3,13],cex=.4)
  # 
  # text(209.8,20.5,subz1[3,14],cex=.4)
  # 
  # text(209.8,18.5,subz1[4,13],cex=.4)
  # 
  # text(209.8,16.5,subz1[4,14],cex=.4)
  # 
  # text(209.8,14.5,subz1[5,13],cex=.4)
  # 
  # text(209.8,12.5,subz1[5,14],cex=.4)
  # 
  # text(209.8,10.5,subz1[6,13],cex=.4)
  # 
  # text(209.8,8.5,subz1[6,14],cex=.4)
  # 
  # text(209.8,6.5,subz1[7,13],cex=.4)
  # 
  # text(209.8,4.5,subz1[7,14],cex=.4)
  # 
  # text(209.8,2.5,subz1[8,13],cex=.4)
  # 
  # text(209.8,0.5,subz1[8,14],cex=.4)
  # 
  
  })
  
  
  ##RENDER FILLED BRACKET 
  observeEvent(input$simulate,{
  output$bracket <- renderPlot({


    x<-seq(0,220,(221/67))
    y<-0:66
    plot(x,y,type="l", col.axis="white", col.lab="white", bty="n",
         axes=F, col="white")
    
    segments(0,c(seq(0,30,2),seq(34,64,2)),20,c(seq(0,30,2),seq(34,64,2)))
    segments(20,c(seq(0,28,4),seq(34,62,4)),20,c(seq(2,30,4),seq(36,64,4)))
    segments(20,c(seq(1,29,4),seq(35,63,4)),40,c(seq(1,29,4),seq(35,63,4)))
    segments(40,c(seq(1,25,8),seq(35,59,8)),40,c(seq(5,29,8),seq(39,63,8)))
    segments(40,c(3,11,19,27,37,45,53,61),60,c(3,11,19,27,37,45,53,61))
    segments(60,c(3,19,37,53),60,c(11,27,45,61))
    segments(60,c(7,23,41,57),80,c(7,23,41,57))
    segments(80,c(7,41),80,c(23,57))
    segments(80,c(15,49),100,c(15,49))
    
    segments(100,c(27,37),120,c(27,37))
    segments(200,c(seq(0,30,2),seq(34,64,2)),220,c(seq(0,30,2),seq(34,64,2)))
    segments(200,c(seq(0,28,4),seq(34,62,4)),200,c(seq(2,30,4),seq(36,64,4)))
    segments(180,c(seq(1,29,4),seq(35,63,4)),200,c(seq(1,29,4),seq(35,63,4)))
    segments(180,c(seq(1,25,8),seq(35,59,8)),180,c(seq(5,29,8),seq(39,63,8)))
    segments(160,c(3,11,19,27,37,45,53,61),180,c(3,11,19,27,37,45,53,61))
    segments(160,c(3,19,37,53),160,c(11,27,45,61))
    segments(140,c(7,23,41,57),160,c(7,23,41,57))
    segments(140,c(7,41),140,c(23,57))
    segments(120,c(15,49),140,c(15,49))
    

    size <- 8.5
    height <- 2
    start <- 65
    j<-16
    k<-16
    m<- 9
    #1st round
    for(i in 1:16){
      if(i==m) break
      text(size, start, str_pad(stats[i,1], width=30, side="right"), cex=.75, font=2)
      text(size, start, str_pad(stats[i,2], width=10, side="both"),cex=.75, font=2)
      start<- start - height
      text(size, start, str_pad(stats[j,1], width=30, side="right"), cex=.75, font=2)
      text(size, start, str_pad(stats[j,2], width=10, side="both"),cex=.75, font=2)
      start<- start - height
      j=j-1
      
    }
    #2nd round
    start2 <-64
    for(i in 1:8){
      text(31, start2, str_pad(stats2[i,1], width=30, side="right"), cex=.75, font=2)
      text(31, start2, str_pad(stats2[i,2], width=6, side="both"),cex=.75, font=2)
      start2<- start2 - 4
    }
    
    start2 <- 62
    for(i in 1:4){
      text(50, start2, str_pad(stats3[i,1], width=30, side="right"), cex=.75, font=2)
      text(50, start2, str_pad(stats3[i,2], width=6, side="both"),cex=.75, font=2)
      start2<- start2 - 8
    }
    

    text(72, 58, str_pad(stats4[1,1], width=30, side="right"), cex=.75, font=2)
    text(72, 58, str_pad(stats4[1,2], width=6, side="both"),cex=.75, font=2)
    
    text(72, 42, str_pad(stats4[2,1], width=30, side="right"), cex=.75, font=2)
    text(72, 42, str_pad(stats4[2,2], width=6, side="both"),cex=.75, font=2)
    # 
    # text(9.8,62.5,subw1[1,14],cex=.4)
    # 
    # text(9.8,60.5,subw1[2,13],cex=.4)
    # 
    # text(9.8,58.5,subw1[2,14],cex=.4)
    # 
    # text(9.8,56.5,subw1[3,13],cex=.4)
    # 
    # text(9.8,54.5,subw1[3,14],cex=.4)
    # 
    # text(9.8,52.5,subw1[4,13],cex=.4)
    # 
    # text(9.8,50.5,subw1[4,14],cex=.4)
    # 
    # text(9.8,48.5,subw1[5,13],cex=.4)
    # 
    # text(9.8,46.5,subw1[5,14],cex=.4)
    # 
    # text(9.8,44.5,subw1[6,13],cex=.4)
    # 
    # text(9.8,42.5,subw1[6,14],cex=.4)
    # 
    # text(9.8,40.5,subw1[7,13],cex=.4)
    # 
    # text(9.8,38.5,subw1[7,14],cex=.4)
    # 
    # text(9.8,36.5,subw1[8,13],cex=.4)
    # 
    # text(9.8,34.5,subw1[8,14],cex=.4)
    # 
    # text(29.8,63.5,"subw2[1,13]",cex=.4)
    # 
    # text(29.8,59.5,subw2[1,14],cex=.4)
    # 
    # text(29.8,55.5,subw2[2,13],cex=.4)
    # 
    # text(29.8,51.5,subw2[2,14],cex=.4)
    # 
    # text(29.8,47.5,subw2[3,13],cex=.4)
    # 
    # text(29.8,43.5,subw2[3,14],cex=.4)
    # 
    # text(29.8,39.5,subw2[4,13],cex=.4)
    # 
    # text(29.8,35.5,subw2[4,14],cex=.4)
    # 
    # text(49.8,61.5,subw3[1,13],cex=.4)
    # 
    # text(49.8,53.5,subw3[1,14],cex=.4)
    # 
    # text(49.8,45.5,subw3[2,13],cex=.4)
    # 
    # text(49.8,37.5,subw3[2,14],cex=.4)
    # 
    # text(69.8,57.5,subw4[1,11],cex=.4)
    # 
    # text(69.8,41.5,subw4[1,12],cex=.4)
    # 
    # text(9.8,30.5,suby1[1,13],cex=.4)
    # 
    # text(9.8,28.5,suby1[1,14],cex=.4)
    # 
    # text(9.8,26.5,suby1[2,13],cex=.4)
    # 
    # text(9.8,24.5,suby1[2,14],cex=.4)
    # 
    # text(9.8,22.5,suby1[3,13],cex=.4)
    # 
    # text(9.8,20.5,suby1[3,14],cex=.4)
    # 
    # text(9.8,18.5,suby1[4,13],cex=.4)
    # 
    # text(9.8,16.5,suby1[4,14],cex=.4)
    # 
    # text(9.8,14.5,suby1[5,13],cex=.4)
    # 
    # text(9.8,12.5,suby1[5,14],cex=.4)
    # 
    # text(9.8,10.5,suby1[6,13],cex=.4)
    # 
    # text(9.8,8.5,suby1[6,14],cex=.4)
    # 
    # text(9.8,6.5,suby1[7,13],cex=.4)
    # 
    # text(9.8,4.5,suby1[7,14],cex=.4)
    # 
    # text(9.8,2.5,suby1[8,13],cex=.4)
    # 
    # text(9.8,0.5,suby1[8,14],cex=.4)
    # 
    # text(29.8,29.5,suby2[1,13],cex=.4)
    # 
    # text(29.8,25.5,suby2[1,14],cex=.4)
    # 
    # text(29.8,21.5,suby2[2,13],cex=.4)
    # 
    # text(29.8,17.5,suby2[2,14],cex=.4)
    # 
    # text(29.8,13.5,suby2[3,13],cex=.4)
    # 
    # text(29.8,9.5,suby2[3,14],cex=.4)
    # 
    # text(29.8,5.5,suby2[4,13],cex=.4)
    # 
    # text(29.8,1.5,suby2[4,14],cex=.4)
    # 
    # text(49.8,27.5,suby3[1,13],cex=.4)
    # 
    # text(49.8,19.5,suby3[1,14],cex=.4)
    # 
    # text(49.8,11.5,suby3[2,13],cex=.4)
    # 
    # text(49.8,3.5,suby3[2,14],cex=.4)
    # 
    # text(69.8,23.5,suby4[1,11],cex=.4)
    # 
    # text(69.8,7.5,suby4[1,12],cex=.4)
    # 
    # text(209.8,64.5,subx1[1,13],cex=.4)
    # 
    # text(209.8,62.5,subx1[1,14],cex=.4)
    # 
    # text(209.8,60.5,subx1[2,13],cex=.4)
    # 
    # text(209.8,58.5,subx1[2,14],cex=.4)
    # 
    # text(209.8,56.5,subx1[3,13],cex=.4)
    # 
    # text(209.8,54.5,subx1[3,14],cex=.4)
    # 
    # text(209.8,52.5,subx1[4,13],cex=.4)
    # 
    # text(209.8,50.5,subx1[4,14],cex=.4)
    # 
    # text(209.8,48.5,subx1[5,13],cex=.4)
    # 
    # text(209.8,46.5,subx1[5,14],cex=.4)
    # 
    # text(209.8,44.5,subx1[6,13],cex=.4)
    # 
    # text(209.8,42.5,subx1[6,14],cex=.4)
    # 
    # text(209.8,40.5,subx1[7,13],cex=.4)
    # 
    # text(209.8,38.5,subx1[7,14],cex=.4)
    # 
    # text(209.8,36.5,subx1[8,13],cex=.4)
    # 
    # text(209.8,34.5,subx1[8,14],cex=.4)
    # 
    # text(189.8,63.5,subx2[1,13],cex=.4)
    # 
    # text(189.8,59.5,subx2[1,14],cex=.4)
    # 
    # text(189.8,55.5,subx2[2,13],cex=.4)
    # 
    # text(189.8,51.5,subx2[2,14],cex=.4)
    # 
    # text(189.8,47.5,subx2[3,13],cex=.4)
    # 
    # text(189.8,43.5,subx2[3,14],cex=.4)
    # 
    # text(189.8,39.5,subx2[4,13],cex=.4)
    # 
    # text(189.8,35.5,subx2[4,14],cex=.4)
    # 
    # text(169.8,61.5,subx3[1,13],cex=.4)
    # 
    # text(169.8,53.5,subx3[1,14],cex=.4)
    # 
    # text(169.8,45.5,subx3[2,13],cex=.4)
    # 
    # text(169.8,37.5,subx3[2,14],cex=.4)
    # 
    # text(149.8,57.5,subx4[1,11],cex=.4)
    # 
    # text(149.8,41.5,subx4[1,12],cex=.4)
    # 
    # text(209.8,30.5,subz1[1,13],cex=.4)
    # 
    # text(209.8,28.5,subz1[1,14],cex=.4)
    # 
    # text(209.8,26.5,subz1[2,13],cex=.4)
    # 
    # text(209.8,24.5,subz1[2,14],cex=.4)
    # 
    # text(209.8,22.5,subz1[3,13],cex=.4)
    # 
    # text(209.8,20.5,subz1[3,14],cex=.4)
    # 
    # text(209.8,18.5,subz1[4,13],cex=.4)
    # 
    # text(209.8,16.5,subz1[4,14],cex=.4)
    # 
    # text(209.8,14.5,subz1[5,13],cex=.4)
    # 
    # text(209.8,12.5,subz1[5,14],cex=.4)
    # 
    # text(209.8,10.5,subz1[6,13],cex=.4)
    # 
    # text(209.8,8.5,subz1[6,14],cex=.4)
    # 
    # text(209.8,6.5,subz1[7,13],cex=.4)
    # 
    # text(209.8,4.5,subz1[7,14],cex=.4)
    # 
    # text(209.8,2.5,subz1[8,13],cex=.4)
    # 
    # text(209.8,0.5,subz1[8,14],cex=.4)
    # 
    # text(189.8,29.5,subz2[1,13],cex=.4)
    # 
    # text(189.8,25.5,subz2[1,14],cex=.4)
    # 
    # text(189.8,21.5,subz2[2,13],cex=.4)
    # 
    # text(189.8,17.5,subz2[2,14],cex=.4)
    # 
    # text(189.8,13.5,subz2[3,13],cex=.4)
    # 
    # text(189.8,9.5,subz2[3,14],cex=.4)
    # 
    # text(189.8,5.5,subz2[4,13],cex=.4)
    # 
    # text(189.8,1.5,subz2[4,14],cex=.4)
    # 
    # text(169.8,27.5,subz3[1,13],cex=.4)
    # 
    # text(169.8,19.5,subz3[1,14],cex=.4)
    # 
    # text(169.8,11.5,subz3[2,13],cex=.4)
    # 
    # text(169.8,3.5,subz3[2,14],cex=.4)
    # 
    # text(149.8,23.5,subz4[1,11],cex=.4)
    # 
    # text(149.8,7.5,subz4[1,12],cex=.4)
    # 
     FinalFour <- ifelse(stats4$Weight[1] > stats4$Weight[2], stats4$Team[1], stats4$Team[2])
     text(91,46.5,paste0((stats %>% filter(Team==FinalFour))$Seed, " ", (stats %>% filter(Team==FinalFour))$Team),cex=1.4, family="mono", font =2)
     img <- readPNG((stats %>% filter(Team==FinalFour))$img)
     rasterImage(img, 82,50,99,65)
    # 
    # text(129.8,49.5,subff[1,13],cex=.4)
    # 
    # text(89.8,15.5,subff[2,12],cex=.4)
    # 
    # text(129.8,15.5,subff[2,13],cex=.4)
    # 
    # text(109.8,37.5,subfinal[1,11],cex=.4)
    # 
    # text(109.8,27.5,subfinal[1,12],cex=.4)
    # 
     text(109.8,32.5,(stats %>% filter(Team==FinalFour))$Team,cex=2.8, font =2, family="mono")
    
    
    
  }
  
  
  
  )
  })

  

  
}