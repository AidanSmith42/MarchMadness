library(shinythemes)
library(shiny)
library(shinydashboard)
navbarPage("BracketBuilder",
           theme=shinytheme("cosmo"),
           tabPanel("Bracket", icon=icon('basketball-ball'),
                 box(width="100%",
                     plotOutput("bracket",height = "600")),
                
              
                box(width="100%", h2(HTML("<center><b>Adjust Weights to Fill Bracket</b></center>")),
                 fluidRow(
                    column(4, align='center',
                           bookmarkButton(label="Share", width="40%", height="20%")),
                    column(4, align="center",
                           actionButton("simulate", "Build Bracket", icon=icon("basketball-ball"), width="100%", height="20%")),

                             column(width=4, align="center",
                              fluidRow(
                                column(6, align="right",
                                    HTML('<div style="float:center">
                                  
                                 <a class="github-button" 
                                   href="https://github.com/AidanSmith42/MarchMadness" 
                                   data-icon="octicon-logo-github" 
                                   data-style="mega" 
                                   data-count-href="/AidanSmith42/MarchMadness/stargazers" 
                                   data-count-api="/repos//AidanSmith42/MarchMadness#stargazers_count" 
                                   data-count-aria-label="# stargazers on GitHub" 
                                   aria-label="Star AidanSmith42/MarchMadness on GitHub">
                                   </a>
                                 <!-- Place this tag in your head or just before your close body tag. -->
                                 <script async defer src="https://buttons.github.io/buttons.js"></script>
                                 </div>')),
                              column(6,align="center",
                              HTML("<div style='float:center'>
                                    <a href='https://twitter.com/share' 
                                    class='twitter-share-button' 
                                    align='middle' 
                                    data-url='https://github.com/AidanSmith42/MarchMadness' 
                                    data-text='Build a customized NCAA Bracket with this app. #ShinyR #marchMadness' 
                                    data-size='large'>Tweet
                                    </a>
                                   <script>!function(d,s,id){
                                    var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';
                                    if(!d.getElementById(id)){
                                    js=d.createElement(s);
                                    js.id=id;
                                    js.src=p+'://platform.twitter.com/widgets.js';
                                    fjs.parentNode.insertBefore(js,fjs);
                                    }
                                    }(document, 'script', 'twitter-wjs');
                                    </script>
                                   </div>")))),
                                      hr()),
           
                    column(4,
                           h2(HTML("<b><center>Per Game Stats</center></b>")),
                           wellPanel(
                           sliderInput("PPG", "Points Per Game", min=0, max=10, value=0, width = "100%"),
                           sliderInput("RPG", "Rebounds Per Game", min=0, max=10, value=0, width = "100%"),
                           sliderInput("APG", "Assists Per Game", min=0, max=10, value=0, width = "100%"),
                           sliderInput("Three", "3 Pt %", min=0, max=10, value=0, width="100%"),
                           sliderInput("SoS", "Strength of Schedule", min=0, max=10, value=0, width = "100%"))),
                    column(4,
                           h2(HTML("<b><center>Efficiency Stats</center></b>")),
                           wellPanel(
                           sliderInput("OEff", "Offense Efficiency", min=0, max=10, value=0, width = "100%"),
                           sliderInput("DEff", "Defense Efficiency", min=0, max=10, value=0, width = "100%"),
                           sliderInput("EffM", "Total Efficiency Margin", min=0, max=10, value=0, width = "100%"),
                           sliderInput("TEff", "Tempo", min=0, max=10, value=0, width = "100%"),
                           sliderInput("WinPer", "Win %", min=0, max=10, value=0, width = "100%"))),
                    column(4,
                           h2(HTML("<b><center>Four Factors</center></b>")),
                           wellPanel(
                           sliderInput("eFG", "Effective Field Goal Percentage", min=0, max=10, value=4, width = "100%"),
                           sliderInput("TOV", "Turnover Percentage", min=0, max=10, value=3, width = "100%"),
                           sliderInput("TRB", "Total Rebound Percentage", min=0, max=10, value=2, width = "100%"),
                           sliderInput("FT", "Free Throw Rate", min=0, max=10, value=2, width = "100%"),
                           sliderInput("Rand", "Randomness", min=0, max=10, value=0, width = "100%")))
                           
                    ),
                box(width="100%",
                    DTOutput("table"))),
           tabPanel("About", icon=icon("book"),
                    box(width="100%",
                      h2(HTML("<b><center> One Shiny Moment </center> </b>")),
                      column(6,
                      wellPanel(
                      p(h4("It's that time of the year again. ", 
                        "This app uses the selected metrics provided from the sliders to build a March Madness bracket.", 
                        "While some of the metrics are straightforward, some of the efficiency metrics could use some definitions.", 
                        br())),
                      HTML("<strong><font family='Light Italic'>Per Game Stats</font></strong>"),
                      p(h4("These stats are relatively straightforward.", "These include the average number of Points, Rebounds, and Assists per game.",
                           "Additionally there is 3P% which is the percentage of 3 Pointers made ( 3PointMade / 3PointAttempts).")),
                      HTML("<strong><font family='Light Italic'>Strength of Schedule</font></strong>"),
                      h4("A standardized rating of strength of schedule.",br()," The rating is denominated in points above/below average, where zero is average. Non-Division I games are excluded from the ratings."),
                      HTML("<strong><font family='Light Italic'>Offensive Efficiency</font></strong>"),
                      p(h4("The following efficiency stats are from renowned basketball statistician Ken Pomeroy (kenpom.com).", br(), "An estimate of the offensive efficiency (points scored per 100 possessions) a team would have against the average D-I defense.")),
                      HTML("<strong><font family='Light Italic'>Defensive Efficiency</font></strong>"),
                      p(h4("An estimate of the defensive efficiency (points allowed per 100 possessions) a team would have against the average D-I offense.")),
                      HTML("<strong><font family='Light Italic'>Total Efficiency Margin</font></strong>"),
                      p(h4("The difference between a team’s offensive and defensive efficiency.", "It represents the number of points the team would be expected to outscore the average D-I team over 100 possessions")),
                      HTML("<strong><font family='Light Italic'>Tempo</font></strong>"),
                      p(h4("An estimate of the tempo (possessions per 40 minutes) a team would have against the team that wants to play at an average D-I tempo.")),
                      HTML("<strong><font family='Light Italic'>Win Percentage</font></strong>"),
                      h4("The number of wins divided by the number of losses.")
                      
                    )
                    ),
                    column(6,
                           wellPanel(
                             HTML("<strong><font family='Light Italic'>Randomness</font></strong>"),
                             p(h4("It's called Madness for a reason. ","Having a winning bracket requires some leprechaun luck. ", "The randomness slider will randomize the selected percent of the teams weights, multiplying them by a random number between 1 and 2.", 
                                  " 1 on the slider would randomize 10% of the teams weights.", br()," You've gotta roll the dice somewhere...")),
                             h4(HTML("<strong><font family='Light Italic'>Four Factors</font></strong>")),
                             p(h4("How do basketball teams win games?",'Statistician Dean Oliver defined the "Four Factors of Basketball Success".',br(), br(), "Shooting (40%)", br(),
                                  "Turnovers (25%)",br(),
                                  "Rebounding (20%)", br(),
                                  "Free Throws (15%)", br())),
                             HTML("<strong><font family='Light Italic'>Effective Field Goal Percentage </font></strong>"),
                             p(h4("The shooting factor is measured using Effective Field Goal Percentage.", "The formula for both offense and defense is (FG + 0.5 * 3P) / FGA")),
                             HTML("<strong><font family='Light Italic'>Turnover Percentage </font></strong>"),
                             p(h4("The turnover factor is measured using Turnover Percentage.", " The formula for both offense and defense is TOV / (FGA + 0.44 * FTA + TOV).")),
                             HTML("<strong><font family='Light Italic'>Total Rebound Percentage</font></strong>"),
                             p(h4("The rebounding factor is measured using Offensive and Defensive Rebound Percentage."," The formula for offense is ORB / (ORB + Opp DRB), while the formula for defense is DRB / (Opp ORB + DRB).")),
                             HTML("<strong><font family='Light Italic'>Free Throw Rate</font></strong>"),
                             p(h4("The free throw factor is a measure of both how often a team gets to the line and how often they make them.", "The formula for both offense and defense is FT / FGA.")),
                             br(),
                             h5(HTML('<p>Data was scraped from
                                    <a href="https://kenpom.com/">kenpom.com</a> <a href="https://www.sports-reference.com/cbb/seasons/2020-advanced-school-stats.html">sports-reference.com/cbb</a></p>'))
                           ))))
)
                