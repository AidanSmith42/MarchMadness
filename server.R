library(readr)
library(tidyverse)
library(png)



setwd(paste0(getwd(), "/www"))
stats <- read_csv("DATA.csv")
playin <- read_csv("playin.csv")

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
                              input$Three/10 * stats$X3R + input$SoS/10 * stats$SoSR + input$OEff/10 * stats$Orank + 
                              input$DEff/10 * stats$Drank + input$EffM/10 * stats$EM + input$TEff/10 * stats$Tempo +
                              input$WinPer/10 * stats$WinPerc + input$eFG/10 * stats$eFGR + input$TOV/10 * stats$TOVR +
                              input$TRB/10 *stats$TRBR + input$FT/10 *stats$FTR)
  

  playin <- playin %>% mutate(Weight = (input$PPG/10)*playin$ppgR + input$RPG/10 * playin$rpgR + input$APG/10 * playin$apgR +
               input$Three/10 * playin$X3R + input$SoS/10 * playin$SoSR + input$OEff/10 * playin$Orank + 
               input$DEff/10 * playin$Drank + input$EffM/10 * playin$EM + input$TEff/10 * playin$Tempo +
               input$WinPer/10 * playin$WinPerc + input$eFG/10 * playin$eFGR + input$TOV/10 * playin$TOVR +
               input$TRB/10 *playin$TRBR + input$FT/10 *playin$FTR)
  

  #Play-in
  if(playin[1,35] > stats[2,35]) stats[2,] <- playin[1,]
  if(playin[2,35] > stats[10,35]) stats[10,] <- playin[2,]
  if(playin[3,35] > stats[18,35]) stats[18,] <- playin[3,]
  if(playin[4,35] > stats[26,35]) stats[26,] <- playin[4,]

  
 #Randomly select percentage of rows and multiply them by a random val between 1,2
 #set seed for reproduceability
  set.seed(69)
  observeEvent(input$Rand, {
  y <- sample(1:nrow(stats), ceiling(input$Rand/10 * nrow(stats)), replace = T)
  set.seed(69)
  stats[y, ]$Weight <- round(stats[y,]$Weight * runif(length(y), 1, 5),1)
  })

  
  stats2 <- data.frame(Seed= numeric(32), Team=character(32), Weight=numeric(32), stringsAsFactors = F)
  j<-1
  for(i in seq(1,16,2)){

    stats2$Seed[j] <- ifelse(stats[i,35]$Weight > stats[i+1,35]$Weight, stats$Seed[i], stats$Seed[i+1])
    stats2$Team[j] <- ifelse(stats[i,35]$Weight > stats[i+1,35]$Weight, stats$Team[i], stats$Team[i+1])
    stats2$Weight[j] <- ifelse(stats[i,35]$Weight > stats[i+1,35]$Weight, stats$Weight[i], stats$Weight[i+1])
    
    stats2$Seed[j+8] <- ifelse(stats[i+16,35]$Weight > stats[i+16+1,35]$Weight, stats$Seed[i+16], stats$Seed[i+16+1])
    stats2$Team[j+8] <- ifelse(stats[i+16,35]$Weight > stats[i+16+1,35]$Weight, stats$Team[i+ 16], stats$Team[i+16+1])
    stats2$Weight[j+8] <- ifelse(stats[i+16,35]$Weight > stats[i+16+1,35]$Weight, stats$Weight[i+16], stats$Weight[i+16+1])
    
    stats2$Seed[j+16] <- ifelse(stats[i+32,35]$Weight > stats[i+32+1,35]$Weight, stats$Seed[i+32], stats$Seed[i+32+1])
    stats2$Team[j+16] <- ifelse(stats[i+32,35]$Weight > stats[i+32+1,35]$Weight, stats$Team[i+ 32], stats$Team[i+32+1])
    stats2$Weight[j+16] <- ifelse(stats[i+32,35]$Weight > stats[i+32+1,35]$Weight, stats$Weight[i+32], stats$Weight[i+32+1])
    
    stats2$Seed[j+24] <- ifelse(stats[i+48,35]$Weight > stats[i+48+1,35]$Weight, stats$Seed[i+48], stats$Seed[i+48+1])
    stats2$Team[j+24] <- ifelse(stats[i+48,35]$Weight > stats[i+48+1,35]$Weight, stats$Team[i+ 48], stats$Team[i+48+1])
    stats2$Weight[j+24] <- ifelse(stats[i+48,35]$Weight > stats[i+48+1,35]$Weight, stats$Weight[i+48], stats$Weight[i+48+1])
    
    j <- j+1
  }
  stats2 <<- stats2

  stats3 <- data.frame(Seed= numeric(16), Team=character(16), Weight=numeric(16), stringsAsFactors = F)
  j <- 1
  for(i in seq(1,8,2)){
    stats3$Seed[j] <- ifelse(stats2$Weight[i] > stats2$Weight[i+1], stats2$Seed[i], stats2$Seed[i+1])
    stats3$Team[j] <- ifelse(stats2$Weight[i] > stats2$Weight[i+1], stats2$Team[i], stats2$Team[i+1])
    stats3$Weight[j] <- ifelse(stats2$Weight[i] > stats2$Weight[i+1], stats2$Weight[i], stats2$Weight[i+1])
    
    stats3$Seed[j+4] <- ifelse(stats2$Weight[i+8] > stats2$Weight[i+8+1], stats2$Seed[i+8], stats2$Seed[i+8+1])
    stats3$Team[j+4] <- ifelse(stats2$Weight[i+8] > stats2$Weight[i+8+1], stats2$Team[i+8], stats2$Team[i+8+1])
    stats3$Weight[j+4] <- ifelse(stats2$Weight[i+8] > stats2$Weight[i+8+1], stats2$Weight[i+8], stats2$Weight[i+8+1])
    
    stats3$Seed[j+8] <- ifelse(stats2$Weight[i + 16] > stats2$Weight[i+16+1], stats2$Seed[i+16], stats2$Seed[i+16+1])
    stats3$Team[j+8] <- ifelse(stats2$Weight[i + 16] > stats2$Weight[i+16+1], stats2$Team[i+16], stats2$Team[i+16+1])
    stats3$Weight[j+8] <- ifelse(stats2$Weight[i + 16] > stats2$Weight[i+16+1], stats2$Weight[i+16], stats2$Weight[i+16+1])
    
    stats3$Seed[j+12] <- ifelse(stats2$Weight[i + 24] > stats2$Weight[i+24+1], stats2$Seed[i+24], stats2$Seed[i+24+1])
    stats3$Team[j+12] <- ifelse(stats2$Weight[i + 24] > stats2$Weight[i+24+1], stats2$Team[i+24], stats2$Team[i+24+1])
    stats3$Weight[j+12] <- ifelse(stats2$Weight[i + 24] > stats2$Weight[i+24+1], stats2$Weight[i+24], stats2$Weight[i+24+1])
    j <- j+1
  }
  
  stats3 <<- stats3
  
  stats4 <- data.frame(Seed= numeric(8), Team=character(8), Weight=numeric(8), stringsAsFactors = F)
  j <- 1
  for(i in seq(1,4,2)){
    stats4$Seed[j] <- ifelse(stats3$Weight[i] > stats3$Weight[i+1], stats3$Seed[i], stats3$Seed[i+1])
    stats4$Team[j] <- ifelse(stats3$Weight[i] > stats3$Weight[i+1], stats3$Team[i], stats3$Team[i+1])
    stats4$Weight[j] <- ifelse(stats3$Weight[i] > stats3$Weight[i+1], stats3$Weight[i], stats3$Weight[i+1])
    
    stats4$Seed[j+2] <- ifelse(stats3$Weight[i+4] > stats3$Weight[i+4+1], stats3$Seed[i+4], stats3$Seed[i+4+1])
    stats4$Team[j+2] <- ifelse(stats3$Weight[i+4] > stats3$Weight[i+4+1], stats3$Team[i+4], stats3$Team[i+4+1])
    stats4$Weight[j+2] <- ifelse(stats3$Weight[i+4] > stats3$Weight[i+4+1], stats3$Weight[i+4], stats3$Weight[i+4+1])
    
    stats4$Seed[j+4] <- ifelse(stats3$Weight[i+8] > stats3$Weight[i+8+1], stats3$Seed[i+8], stats3$Seed[i+8+1])
    stats4$Team[j+4] <- ifelse(stats3$Weight[i+8] > stats3$Weight[i+8+1], stats3$Team[i+8], stats3$Team[i+8+1])
    stats4$Weight[j+4] <- ifelse(stats3$Weight[i+8] > stats3$Weight[i+8+1], stats3$Weight[i+8], stats3$Weight[i+8+1])
    
    stats4$Seed[j+6] <- ifelse(stats3$Weight[i+12] > stats3$Weight[i+12+1], stats3$Seed[i+12], stats3$Seed[i+12+1])
    stats4$Team[j+6] <- ifelse(stats3$Weight[i+12] > stats3$Weight[i+12+1], stats3$Team[i+12], stats3$Team[i+12+1])
    stats4$Weight[j+6] <- ifelse(stats3$Weight[i+12] > stats3$Weight[i+12+1], stats3$Weight[i+12], stats3$Weight[i+12+1])
    j <- j+1
  }
  
  stats4 <<- stats4
  


  output$table <- renderDT(datatable(stats %>% 
                           select(-c(WinPerc, EM, Orank,Drank,Tempo,ppgR, rpgR,apgR,X3R,SoSR, eFGR, TOVR, TRBR, FTR, img)) %>% 
                           arrange(desc(Weight)),
                           extensions='ColReorder', options=list(colReorder=T, pageLength= 64, scrollY = 600), rownames = F))
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
  
  size <- 9.5
  height <- 2
  start <- 65
  
  for(i in seq(1,16,2)){
    text(size, start, str_pad(stats[i,1], width=30, side="right"), cex=.75, font=2)
    text(size,start,str_pad(stats[i,2], width=10, side="both"),cex=.75, font=2)
    start<- start - height
    text(size, start, str_pad(stats[i+1,1], width=30, side="right"), cex=.75, font=2)
    text(size,start,str_pad(stats[i+1,2], width=10, side="both"),cex=.75, font=2)
    start<- start - height

    
  }
  
  q2 <- 30.75
  for(i in seq(1,16,2)){
    text(size, q2, str_pad(stats[16+i,1], width=30, side="right"), cex=.75, font=2)
    text(size,q2,str_pad(stats[16+i,2], width=10, side="both"),cex=.75, font=2)
    q2<- q2 - height
    text(size, q2, str_pad(stats[16+i+1,1], width=30, side="right"), cex=.75, font=2)
    text(size,q2,str_pad(stats[16+i+1,2], width=10, side="both"),cex=.75, font=2)
    q2<- q2 - height
    
    
  }
  
  q3 <- 65
  size <- 217.5
  for(i in seq(1,16,2)){
    text(size, q3, str_pad(stats[32+i,1], width=15, side="left"), cex=.75, font=2)
    text(size,q3,str_pad(stats[32+i,2], width=30, side="right"),cex=.75, font=2)
    q3<- q3 - height
    text(size, q3, str_pad(stats[32+i+1,1], width=15, side="left"), cex=.75, font=2)
    text(size,q3,str_pad(stats[32+i+1,2], width=30, side="right"),cex=.75, font=2)
    q3<- q3 - height
    
    
  }
  q4 <- 30.75
  for(i in seq(1,16,2)){
    text(size, q4, str_pad(stats[48+i,1], width=15, side="left"), cex=.75, font=2)
    text(size,q4,str_pad(stats[48+i,2], width=30, side="right"),cex=.75, font=2)
    q4<- q4 - height
    text(size, q4, str_pad(stats[48+i+1,1], width=15, side="left"), cex=.75, font=2)
    text(size,q4,str_pad(stats[48+i+1,2], width=30, side="right"),cex=.75, font=2)
    q4<- q4 - height
    
    
  }

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
    q2 <- 30.75
    q3 <- 65
    q4 <- 30.75
    #1st round
    for(i in seq(1,16,2)){
      text(size, start, str_pad(stats[i,1], width=30, side="right"), cex=.75, font=2)
      text(size, start, str_pad(stats[i,2], width=10, side="both"),cex=.75, font=2)
      start<- start - height
      text(size, start, str_pad(stats[i+1,1], width=30, side="right"), cex=.75, font=2)
      text(size, start, str_pad(stats[i+1,2], width=10, side="both"),cex=.75, font=2)
      start<- start - height
 
      text(size, q2, str_pad(stats[16+i,1], width=30, side="right"), cex=.75, font=2)
      text(size,q2,str_pad(stats[16+i,2], width=10, side="both"),cex=.75, font=2)
      q2<- q2 - height
      text(size, q2, str_pad(stats[16+i+1,1], width=30, side="right"), cex=.75, font=2)
      text(size,q2,str_pad(stats[16+i+1,2], width=10, side="both"),cex=.75, font=2)
      q2<- q2 - height
      
      text(217.5, q3, str_pad(stats[32+i,1], width=15, side="left"), cex=.75, font=2)
      text(217.5,q3,str_pad(stats[32+i,2], width=30, side="right"),cex=.75, font=2)
      q3<- q3 - height
      text(217.5, q3, str_pad(stats[32+i+1,1], width=15, side="left"), cex=.75, font=2)
      text(217.5,q3,str_pad(stats[32+i+1,2], width=30, side="right"),cex=.75, font=2)
      q3<- q3 - height
      
      text(217.5, q4, str_pad(stats[48+i,1], width=15, side="left"), cex=.75, font=2)
      text(217.5,q4,str_pad(stats[48+i,2], width=30, side="right"),cex=.75, font=2)
      q4<- q4 - height
      text(217.5, q4, str_pad(stats[48+i+1,1], width=15, side="left"), cex=.75, font=2)
      text(217.5,q4,str_pad(stats[48+i+1,2], width=30, side="right"),cex=.75, font=2)
      q4<- q4 - height
      
      
      
    }

 
    #2nd round
    start2 <-64
    temp <- 64
    temp2 <- 30
    temp3 <- 30

    for(i in seq(1,8,2)){
      text(31, temp, str_pad(stats2[i,1], width=30, side="right"), cex=.75, font=2)
      text(31, temp, str_pad(stats2[i,2], width=6, side="both"),cex=.75, font=2)
      temp <- temp - 4
      text(31, temp, str_pad(stats2[i+1,1], width=30, side="right"), cex=.75, font=2)
      text(31, temp, str_pad(stats2[i+1,2], width=6, side="both"),cex=.75, font=2)
      temp <- temp - 4
      text(31, temp2, str_pad(stats2[i+8,1], width=30, side="right"), cex=.75, font=2)
      text(31, temp2, str_pad(stats2[i+8,2], width=6, side="both"),cex=.75, font=2)
      temp2 <- temp2 -4
      text(31, temp2, str_pad(stats2[i+8+1,1], width=30, side="right"), cex=.75, font=2)
      text(31, temp2, str_pad(stats2[i+8+1,2], width=6, side="both"),cex=.75, font=2)
      temp2 <- temp2 -4
      
      text(196, start2, str_pad(stats2[16+i,1], width=6, side="left"), cex=.75, font=2)
      text(196, start2 ,str_pad(stats2[16+i,2], width=30, side="right"),cex=.75, font=2)
      start2 <- start2-4
      text(196, start2, str_pad(stats2[16+i+1,1], width=6, side="left"), cex=.75, font=2)
      text(196, start2 ,str_pad(stats2[16+i+1,2], width=30, side="right"),cex=.75, font=2)
      start2 <- start2 -4
      text(196, temp3, str_pad(stats2[24+i,1], width=6, side="left"), cex=.75, font=2)
      text(196, temp3 ,str_pad(stats2[24+i,2], width=30, side="right"),cex=.75, font=2)
      temp3 <- temp3 -4
      text(196, temp3, str_pad(stats2[24+i+1,1], width=6, side="left"), cex=.75, font=2)
      text(196, temp3 ,str_pad(stats2[24+i+1,2], width=30, side="right"),cex=.75, font=2)
      temp3 <- temp3 -4
      

    }
    
    
    
    start2 <- 62
    temp <- 62
    temp1 <- 28
    temp2 <- 28
    for(i in seq(1,4,2)){
      text(50, start2, str_pad(stats3[i,1], width=30, side="right"), cex=.75, font=2)
      text(50, start2, str_pad(stats3[i,2], width=6, side="left"),cex=.75, font=2)
      start2 <- start2 - 8
      text(50, start2, str_pad(stats3[i+1,1], width=30, side="right"), cex=.75, font=2)
      text(50, start2, str_pad(stats3[i+1,2], width=6, side="left"),cex=.75, font=2)
      start2 <- start2 - 8
      text(50, temp1, str_pad(stats3[i+4,1], width=30, side="right"), cex=.75, font=2)
      text(50, temp1, str_pad(stats3[i+4,2], width=6, side="left"),cex=.75, font=2)
      temp1 <- temp1 -8
      text(50, temp1, str_pad(stats3[i+4+1,1], width=30, side="right"), cex=.75, font=2)
      text(50, temp1, str_pad(stats3[i+4+1,2], width=6, side="left"),cex=.75, font=2)
      temp1 <- temp1 -8
      text(174, temp, str_pad(stats3[i+8,2], width=30, side="right"), cex=.75, font=2)
      text(174, temp, str_pad(stats3[i+8,1], width=6, side="left"),cex=.75, font=2)
      temp <- temp - 8
      text(174, temp, str_pad(stats3[i+8+1,2], width=30, side="right"), cex=.75, font=2)
      text(174, temp, str_pad(stats3[i+8+1,1], width=6, side="left"),cex=.75, font=2)
      temp <- temp - 8
      
      text(174, temp2, str_pad(stats3[i+12,2], width=30, side="right"), cex=.75, font=2)
      text(174, temp2, str_pad(stats3[i+12,1], width=6, side="left"),cex=.75, font=2)
      temp2<- temp2 - 8
      text(174, temp2, str_pad(stats3[i+12+1,2], width=30, side="right"), cex=.75, font=2)
      text(174, temp2, str_pad(stats3[i+12+1,1], width=6, side="left"),cex=.75, font=2)
      temp2<- temp2 - 8
    }
    

    text(72, 58, str_pad(stats4[1,1], width=30, side="right"), cex=.75, font=2)
    text(72, 58, str_pad(stats4[1,2], width=6, side="both"),cex=.75, font=2)
    text(72, 42, str_pad(stats4[2,1], width=30, side="right"), cex=.75, font=2)
    text(72, 42, str_pad(stats4[2,2], width=6, side="both"),cex=.75, font=2)
    text(72, 24, str_pad(stats4[3,1], width=30, side="right"), cex=.75, font=2)
    text(72, 24, str_pad(stats4[3,2], width=6, side="both"),cex=.75, font=2)
    text(72, 8, str_pad(stats4[4,1], width=30, side="right"), cex=.75, font=2)
    text(72, 8, str_pad(stats4[4,2], width=6, side="both"),cex=.75, font=2)
    text(152, 58, str_pad(stats4[5,2], width=30, side="right"), cex=.75, font=2)
    text(152, 58, str_pad(stats4[5,1], width=6, side="left"),cex=.75, font=2)
    text(152, 42, str_pad(stats4[6,2], width=30, side="right"), cex=.75, font=2)
    text(152, 42, str_pad(stats4[6,1], width=6, side="left"),cex=.75, font=2)
    text(152, 24, str_pad(stats4[7,2], width=30, side="right"), cex=.75, font=2)
    text(152, 24, str_pad(stats4[7,1], width=6, side="left"),cex=.75, font=2)
    text(152, 8, str_pad(stats4[8,2], width=30, side="right"), cex=.75, font=2)
    text(152, 8, str_pad(stats4[8,1], width=6, side="left"),cex=.75, font=2)

    
    FinalFour <- character(4)
    FinalFour[1] <- ifelse(stats4$Weight[1] > stats4$Weight[2], stats4$Team[1], stats4$Team[2])
    FinalFour[2] <- ifelse(stats4$Weight[3] > stats4$Weight[4], stats4$Team[3], stats4$Team[4])
    FinalFour[3] <- ifelse(stats4$Weight[5] > stats4$Weight[6], stats4$Team[5], stats4$Team[6])
    FinalFour[4] <- ifelse(stats4$Weight[7] > stats4$Weight[8], stats4$Team[7], stats4$Team[8])
    
    text(91,46.5,paste0((stats %>% filter(Team==FinalFour[1]))$Seed, " ", (stats %>% filter(Team==FinalFour[1]))$Team),cex=1.4, family="mono", font =2)
    img <- readPNG((stats %>% filter(Team==FinalFour[1]))$img)
    rasterImage(img, 82,50,99,65)
    

    text(91,16.5,paste0((stats %>% filter(Team==FinalFour[2]))$Seed, " ", (stats %>% filter(Team==FinalFour[2]))$Team),cex=1.4, family="mono", font =2)
    img <- readPNG((stats %>% filter(Team==FinalFour[2]))$img)
    rasterImage(img, 82,0,99,15)
    text(129,46.5,paste0((stats %>% filter(Team==FinalFour[3]))$Team, " ", (stats %>% filter(Team==FinalFour[3]))$Seed),cex=1.4, family="mono", font =2)
    text(129,16.5,paste0((stats %>% filter(Team==FinalFour[4]))$Team, " ", (stats %>% filter(Team==FinalFour[4]))$Seed),cex=1.4, family="mono", font =2)
    
    img <- readPNG((stats %>% filter(Team==FinalFour[4]))$img)
    rasterImage(img, 120,0,137,15)
    img <- readPNG((stats %>% filter(Team==FinalFour[3]))$img)
    rasterImage(img, 120,50,137,65)
    
 

    Finals1 <- ifelse((stats %>% filter(Team==FinalFour[1]) %>% select(Weight)) > (stats %>% filter(Team==FinalFour[2]) %>% select(Weight)),
                        (stats %>% filter(Team==FinalFour[1]) %>% select(Team)), (stats %>% filter(Team==FinalFour[2]) %>% select(Team)))
    
    Finals2 <- ifelse((stats %>% filter(Team==FinalFour[3]) %>% select(Weight)) > (stats %>% filter(Team==FinalFour[4]) %>% select(Weight)),
                      (stats %>% filter(Team==FinalFour[3]) %>% select(Team)), (stats %>% filter(Team==FinalFour[4]) %>% select(Team)))
    

    

    text(109.8, 39.5, str_pad((stats %>% filter(Team==Finals1[[1]]))$Team, width=6, side="both"),cex=1.4, font=2, family="mono")
    text(109.8, 24.5, str_pad((stats %>% filter(Team==Finals2[[1]]))$Team, width=6, side="both"),cex=1.4, font=2, family="mono")
    
    
    Finals <- stats2 %>% filter(Team %in% c(Finals1[[1]], Finals2[[1]])) %>% arrange(desc(Weight))
    
    
    text(109.8,32.5,(Finals[1,2]),cex=2.8, font =2, family="mono")
    
    
    
  }
  
  
  
  )
  })

  

  
}