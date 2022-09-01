filePath = 'scoreFiles'
finaldata = read.csv("finaldata.csv",header=T)

set.seed(99)
nall <- nrow(finaldata)
npublic <- floor(0.3*nall)
nprivate <- floor(0.7*nall)
index <- seq(1:nall)
public <- sample(index,npublic)
newindex <- index[-public]
private <- newindex


library(shiny)
library(rdrop2)
library(shinydashboard)
#change maximum file size to 200 MB
options(shiny.maxRequestSize = 200*1024^2)
library(dplyr)
library(data.table)
#library(ggplot2)
#library(plotly)
#library(ggthemes)
#library(googlesheets)
#library(lubridate)
library(DT)
library(caret)
library(e1071)
#require(RCurl)

#myCsv=getURL("https://docs.google.com/spreadsheets/d/1fafcphCY6JOgiQXiANQVw_mEKmOKyM9EscUp5pib53I/pub?output=csv")

token <- readRDS("droptoken.rds")
mydat=drop_read_csv('scoreFiles/mydat.csv',dtoken=token)
pvt_scoredata = drop_read_csv('scoreFiles/private_scoredata.csv',dtoken=token)
public_scoredata = drop_read_csv('scoreFiles/public_leaderboard.csv',dtoken=token)
# is.logged <-reactiveValues(isLogged=FALSE)
# userSub <- reactiveValues(alreadySubmitted=FALSE)
# finalSub <- reactiveValues(made=FALSE)
################# ui ########################
ui <- dashboardPage(skin='purple',
                    dashboardHeader(title='Rang Technologies Competition'),
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Summary", tabName='summary', icon=icon('first-order')),
                            menuItem("Winners and Toppers", tabName='winners',icon=icon('trophy')),
                            menuItem("Winners Speak", tabName='winnersSpeak', icon=icon('comments-o'),
                                     menuSubItem("1st Prize Winners",tabName='firstWinner'),
                                     menuSubItem("2nd Prize Winners", tabName='secondWinner'),
                                     menuSubItem("3rd Prize Winner", tabName='thirdWinner')
                                     ),
                            menuItem("Visualization",tabName='visualization',icon=icon('paint-brush'),
                                     menuSubItem("Team Neophyte", tabName="neophyte"),
                                     menuSubItem("Rajasekhar Jetty",tabName='jetty')
                                                 ),
#                            menuItem("Login",tabName='loggin', icon = icon("hand-pointer-o")#,
                                     #selectInput('username','Group Name', choices=as.list(as.character(mydat$Group.Name))),
                                     #fileInput('file1','File',accept=c('text/csv','.csv'))
                            #),
#                            menuItem(text = 'Final Submission', tabName='finalSubmission',icon=icon('skyatlas')),
                            menuItem(text='Private Leaderboard',tabName='pvtLeaderboard',icon=icon('certificate')),
                            menuItem(text='Public Leaderboard', tabName='pubLeaderboard',icon=icon('street-view')),
                            menuItem(text='Participants', tabName='participants',icon=icon('users')),
                            menuItem('Contact Us', tabName='contact', icon=icon('at'))
                    
                        )#End of dabhboard sidebarMenu
                    ), #end of dashboardSidebar
                    dashboardBody(
                        tabItems(
                            tabItem(tabName='summary',
                                    box(width=10,
                                        HTML("
National level data mining competition-2016 has come to end. This is first time such data mining competition occurred at national level between several masters level students across USA. We gave a classification problem and ran the competition in similar lines of Kaggle® competitions. This is a two months long Data Mining/Data Science competition started on April 1st 2016, conducted by Rang Technologies and KVRA tech , for which 279 groups of students from several universities have registered. 95 Groups made several hundred attempts over 2 months period to be on the top. Here are the winners.
                                        "),
                                    br(),
                                    br(),
                                    img(src="winners_image.png",align='center'),
                                    br(),
                                    br(),
                                    p('Here is the summary of participants'),
                                    img(src="visualization/neophyte/digital_metrics.png",align='center'),
                                    br(),
                                    p("Congratulations winners and toppers ! Winners also mentioned how they won the competition to help others to win future competition. Thank to you all students who participated in this. Thanks to professors and career advisors for encouraging the students to be part of this activity."),
                                    br(),
                                    h3("Sponsored By"),
                                    img(src="unnamed.png",align='center'),
                                    #br(),
                                    #img(src='Rewards.png',align='center'),
                                    #p("Timeline for competition"),
                                    #img(src='timeline5.png',align='center'),
                                    HTML('<br><br><div align="Right">
  Kaggle® prototype of Shiny R application developed by Sai Krishna Gaddam ( Drexel Alumnus ) under guidance of Satish Kumar Simhadri ( Rang Technologies inc )
                                         </div>')
                                    )
                                    ),
                            tabItem(tabName='winners',
                                    box(width=10,
                                        HTML("
                                             <p><strong>Winners:</strong> 1<sup>st</sup> , 2<sup>nd</sup> and 3<sup>rd</sup> Prize winners</p>
                                             <p><strong>Toppers :</strong>&nbsp;Of the 95 Participants ( who made at least one submission in the public leaderboard, irrespective whether they opted for final submission to the private leaderboard or not). After excluding top 3 groups, the top 10% and 20% were awarded this honor.</p>
                                             <p><span style='background-color: #ffff00;'><strong>Winners will receive prize money along with certifications, and toppers will get the certificates. Approximately in next 3 to 4 weeks. Congratulations again!</strong></span></p>
                                             ")),
                                    dataTableOutput('winnersTable')),
#                             tabItem(tabName='loggin',
#                                     uiOutput('userInput'),
#                                     uiOutput('passwordIntake'),
#                                     uiOutput('submitIntake'),
#                                     p("If you forget your password, send us an email at career@rangtech.com with subject CASE PASSWORD and we will get back to you in 24 hours!"),
#                                     hr(),
#                                     verbatimTextOutput('submission')
#                             ),

                            
#                             tabItem(tabName = 'finalSubmission',
#                                     shinyjs::useShinyjs(),
#                                     h3("Please wait till scores appear!"),
#                                     uiOutput('finSub'),
#                                     actionButton(inputId = 'finalSubmit',label='Make Final Submission',icon=icon('check')),
#                                     shinyjs::hidden(
#                                         span(id = "submit_msg", "Submitting..."),
#                                         div(id = "error",
#                                             div(br(), tags$b("Error: "), span(id = "error_msg"))
#                                         )
#                                     ),
#                                     shinyjs::hidden(
#                                         div(
#                                             id = "thankyou_msg",
#                                             h3("Thank you, You have made your final submission!")
#                                             #actionLink("submit_another", "Submit another response")
#                                         )
#                                     )
#                                     ),

                            tabItem(tabName='firstWinner',
                                    img(src='first/xxy.png',align='left',width=400,height=400),
                                    img(src='first/huisu.png',align='center',width=400,height=400),
                                    img(src='first/chy.png',width=400,height=400),
                                    br(),
                                    br(),
                                    box("First Prize Winners' Response",witdh=20,
                                        HTML("
<p><strong>1) What tools (SAS Base/SAS miner/ Matlab/R/Python/Any other) did you use to solve the problem?</strong>&nbsp;</p>
<p>We used Python with scikit-learn, Xgboost, Keras with Theano packages.</p>
                                             <p>&nbsp;</p>
                                             <p><strong>2) What are the major challenges in solving this problem and what made you succeed in this competition?</strong></p>
                                             <p>&nbsp;There are several challenges but we gained lot of knowledge after overcoming them:</p>
                                             <p>&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; i) Understanding, cleaning and subsetting the data:</p>
                                             <p>We were not provided with background introduction to the data. So, we spent some time looking into</p>
                                             <p>each variable trying to find a pattern. Fortunately, we found some correlation between variables. We then</p>
                                             <p>grouped them together and reduced the number of variables.</p>
                                             <p>&nbsp;We cannot use the features directly, so we did feature engineering very carefully for each feature. For</p>
                                             <p>features which had outliers we eliminated the outliers. For features which had a wide range of values we</p>
                                             <p>performed the square root transformation on them. For some data, we also found that the NAs&nbsp;occur in all</p>
                                             <p>the records about food so we decided to train separate models on the data containing food NAs and those</p>
                                             <p>without NAs, We did lot of work on feature transformation and engineering.</p>
                                             <p>&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;</p>
                                             <p>&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;ii. Add new features:</p>
                                             <p>We tried with the variables from the data but couldn&rsquo;t make much progress beyond the point when we hit</p>
                                             <p>approximately 68% on public leader board. One teammate found a paper describing some interesting</p>
                                             <p>features to be used in the customer classification using transaction data. In that paper the authors</p>
                                             <p>introduced the variable &amp;quot;number of NAs and number of 0 for each customers'; is quite important for final</p>
                                             <p>prediction of the active customer, so we added these features to our result and the model gave the result</p>
                                             <p>that helped us go beyond 69%.</p>
                                             <p>&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; iii. Ensemble methods:</p>
                                             <p>We first tried a single model, but that stopped at around 69%. After that we tried to combine 13 kinds of</p>
                                             <p>models with both parametric and non-parametric machine learning methods. Based on these prediction</p>
                                             <p>models, we used the 2-layer 5-fold stacking method ensemble. We take the output of the first-layer</p>
                                             <p>models and then combine the original data to train the&nbsp;neural network models in the second layer. It</p>
                                             <p>boosts the accuracy of the result above 70%.&nbsp;</p>
                                             <p>&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;</p>
                                             <p>Feature engineering, feature construction and ensemble learning method with stacking are the most</p>
                                             <p>important parts in our model.</p>
                                             <p>&nbsp;</p>
                                             <p><strong>3)&nbsp;What do you think about level of difficulty of this competition that Rang Technologies analytics team</strong></p>
                                             <p><strong>presented to you?</strong></p>
                                             <p>The competition was very challenging because the size of the data was not very large and we were</p>
                                             <p>compelled to use the limit data to train the accurate model. The major problem we encountered was</p>
                                             <p>overfitting due to the limit size of the data, so we&nbsp;used&nbsp;the bagging method, stacking method and</p>
                                             <p>ensemble learning to overcome that. Also, the meaning of the feature and the background of the data</p>
                                             <p>was&nbsp;not very clear, so we had to spend lot of time in understanding the data and engineer the features.</p>
                                             <p>&nbsp;</p>
                                             <p><strong>4) It is two months long competition, how were you able to maintain the interest?</strong></p>
                                             <p>We actually did not spend the whole of 2 months for this competition because of our school</p>
                                             <p>coursework.&nbsp;We focused on solving the problem within 2 weeks. Moreover, the leader of our team is very</p>
                                             <p>active and did lot of work to set an example for the rest of the team. And to be honest, the prize was really</p>
                                             <p>attractive and our chances of winning really motivated us.</p>
                                             <p>&nbsp;</p>
                                             <p><strong>5)&nbsp; What do you think of this opportunity to compete nationally with all schools across USA?</strong></p>
                                             <p>We think it is a great opportunity to see how well we can do across the nation and our result really</p>
                                             <p>inspired us. However, I believe that if more schools were involved, this competition would have become</p>
                                             <p>more challenging and more competitive.</p>
                                             <p>&nbsp;</p>
                                             <p><strong>6)&nbsp;Will you recommend to your friends, juniors or anyone who wants to pursue analytics as a career to</strong></p>
                                             <p><strong>participate in Rang Technologies National Analytics competition?</strong></p>
                                             <p>Definitely. This is a great opportunity to gain some hands-on experience in data analytics, beyond what</p>
                                             <p>we learn in data mining/ machine learning/ analytics course. We are happy about our achievement and the</p>
                                             <p>hacking skills we gained. This can be a highlight on our resumes and we can talk about this in our future</p>
                                             <p>interviews. Moreover, we are very happy with the codes we wrote, because we can reuse the code and</p>
                                             <p>make some&nbsp;generalizations&nbsp;for further data analytics tasks.</p>
                                             "))),

                            tabItem(tabName='secondWinner',
                                    img(src='second/Vidya_Upendra.png',align='center',width=600,height=400),
                                    br(),
                                    br(),
                                    box(title="Second Prize Winners' Response",
                                        HTML("
<p><strong>1)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; What tools (SAS Base/SAS miner/ Matlab/R/Python/Any other) did you use to solve the</strong></p>
<p><strong>problem?</strong></p>
                                             <ul>
                                             <li>&nbsp;We have used SAS miner for the competition</li>
                                             </ul>
                                             <p>&nbsp;</p>
                                             <p><strong>2)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; What are the major challenges in solving this problem and what made you succeed in this</strong></p>
                                             <p><strong>competition?</strong></p>
                                             <ul>
                                             <li>We are graduate students from Industrial Engineering with little experience in data</li>
                                             </ul>
                                             <p>analytics. Initially it was tough for us to understand the data sets because of its size and</p>
                                             <p>number of variables.</p>
                                             <ul>
                                             <li>&nbsp;We strict to the basics which have been learnt through our course work. We tried to</li>
                                             </ul>
                                             <p>understand the data as much as possible before we could model the data. Various tests</p>
                                             <p>have been performed to understand the correlation between variables.</p>
                                             <ul>
                                             <li>We think understanding the data better and constantly trying different models helped us</li>
                                             </ul>
                                             <p>to succeed in this competition.</p>
                                             <p>&nbsp;</p>
                                             <p><strong>3)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; What do you think about the level of difficulty of this competition that Rang Technologies</strong></p>
                                             <p><strong>analytics team presented to you?</strong></p>
                                             <ul>
                                             <li>As beginners, we felt the level of difficulty was advanced.</li>
                                             </ul>
                                             <p>&nbsp;</p>
                                             <p><strong>4)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; It is two months long competition, how were you able to maintain the interest?</strong></p>
                                             <ul>
                                             <li>&nbsp;We were very passionate about this competition right from the beginning. We joined the</li>
                                             </ul>
                                             <p>competition little late and it was our first competition in data analytics. We wanted to</p>
                                             <p>make it big. The passion to constantly trying to build new models and trying to learn new</p>
                                             <p>concepts kept our interests moving. This project has actually improved our analytics</p>
                                             <p>skills and helped us to broaden our thinking. Moreover, the leaderboard boosted us to</p>
                                             <p>stay in the competition as we were standing first for most of the time in the competition.</p>
                                             <p>&nbsp;</p>
                                             <p><strong>5)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; What do you think of this opportunity to compete Nationally with all schools across USA?</strong></p>
                                             <ul>
                                             <li>&nbsp;It is a great feeling participating in a competition nationally and stand second in the</li>
                                             </ul>
                                             <p>competition. Moreover, we are international students and it boosts our morale winning</p>
                                             <p>such competitions.</p>
                                             <p>&nbsp;</p>
                                             <p><strong>6)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Will you recommend to your friends, juniors or anyone who wants to pursue analytics as</strong></p>
                                             <p><strong> a career to participate in Rang Technologies National Analytics competition?</strong></p>
                                             <ul>
                                             <li>We will definitely recommend to friends and juniors who has interest and passion</li>
                                             </ul>
                                             <p>towards analytics career.</p>
                                             "))),
                            
                            tabItem(tabName='thirdWinner',
                                    img(src='third/Udaychandar.png',align='center',width=400,height=400),
                                    br(),
                                    br(),
                                    box(title="Third Prize Winners' Response",
                                        HTML("
<p><strong>About Me, My Reasoning and Approach:</strong></p>
<p>First I would like to express my sincere thanks to Rang Technologies for organizing such a challenging</p>
                                             <p>and interesting competition and giving the student community an opportunity to showcase their skills in</p>
                                             <p>analytics.</p>
                                             <p>Finishing third in this competition was a special achievement for me since I was competing with very</p>
                                             <p>talented participants and this was my maiden competition. What really motivates me is the fact that I was</p>
                                             <p>able to perform well and win while competing with many students with good educational background and</p>
                                             <p>experience in analytics and IT.</p>
                                             <p>I always look at the problem objectively and look for the simplest way to solve it. This approach has</p>
                                             <p>always helped me in problem-solving and this competition was no different. Analytics is one area where</p>
                                             <p>there is no single answer and no single path to arrive at an answer. So, I used various statistical models</p>
                                             <p>and machine learning algorithms in my earnest attempt to solve the problem at hand and selected the</p>
                                             <p>model that performed better than the rest of the models.</p>
                                             <p>After spending 7 years with Toyota India in sales and marketing, I decided to enter the core analytics field</p>
                                             <p>and ergo, I started my Masters in Business Analytics at University of Texas at Dallas in 2015 Fall. I&rsquo;m now</p>
                                             <p>in my graduating semester (summer&rsquo;16) and the journey of analytics for this last 10 months at UTD has</p>
                                             <p>transformed me a lot as a professional by enriching me with various analytical tools and techniques. The</p>
                                             <p>experience I gained through various in- class projects in my coursework was a major helping hand while</p>
                                             <p>solving this problem.</p>
                                             <p>When I received email from my professor with the details of this competition, I immediately decided to be</p>
                                             <p>a part of it and jumped in the race like a lone wolf with the objective of challenging myself. Participating in</p>
                                             <p>this competition has given me abundant confidence to attempt and attack the real world analytical</p>
                                             <p>problems. Although this has been a great learning experience for me it&rsquo;s still long way ahead in the path of</p>
                                             <p>learning analytics. I thank my professors and peers from whom I learnt a lot. I hope my achievement will</p>
                                             <p>encourage many aspiring analytics professionals like myself to try and push boundaries of their abilities.</p>
                                             <p>Once again I thank Rang for providing a platform to showcase my skills and boosting the confidence of</p>
                                             <p>many like me.</p>
                                             <p>&nbsp;</p>
                                             <p><strong>Questions and Answers:</strong></p>
                                             <p><strong>1) What tools (SAS Base/SAS miner/ Matlab/R/Python/Any other) did you use to solve the</strong></p>
                                             <p><strong>problem?</strong></p>
                                             <p>I used SAS Base and SAS Miner to solve this problem. I found best results with SAS Miner.</p>
                                             <p>&nbsp;</p>
                                             <p><strong>2) What are the major challenges in solving this problem and what made you succeed in this</strong></p>
                                             <p><strong>competition?</strong></p>
                                             <p>I would say the reason for my success is smart work rather than hard work. I always look for the</p>
                                             <p>simplest way to get best results in minimal time, because optimization of time and quality of work</p>
                                             <p>are what matter the most. The major challenge in this problem is improving the model</p>
                                             <p>performance beyond a certain level rather than just solving it.</p>
                                             <p>&nbsp;</p>
                                             <p><strong>3) What do you think about the level of difficulty of this competition that Rang Technologies analytics</strong></p>
                                             <p><strong>the team presented to you?</strong></p>
                                             <p>The competition is at the right level of difficulty for the students and aspiring analytics</p>
                                             <p>professionals</p>
                                             <p>&nbsp;</p>
                                             <p><strong>4) It is two months long competition, how were you able to maintain the interest?</strong></p>
                                             <p>It&rsquo;s really challenging to maintain the initial excitement as the duration is really long. I would</p>
                                             <p>recommend that the duration can be reduced to 1 month length in the future. I believe that we</p>
                                             <p>wouldn&rsquo;t be given such a long span of time to solve a problem of this level in real world.</p>
                                             <p>&nbsp;</p>
                                             <p><strong>5) What do you think of this opportunity to compete nationally with all schools across USA?</strong></p>
                                             <p>It is always really an awesome and exciting feeling to compete with fellow students across the</p>
                                             <p>schools, especially in such national level competitions. These competitions boost our confidence</p>
                                             <p>to face the challenging problems and prepare ourselves to enter the job market. Also, it gives us</p>
                                             <p>an opportunity to gauge our capabilities and skill levels.</p>
                                             <p>&nbsp;</p>
                                             <p><strong>6) Will you recommend to your friends, juniors or anyone who wants to pursue analytics as a career to</strong></p>
                                             <p><strong>participate in Rang Technologies National Analytics competition?</strong></p>
                                             <p>Yes. I would strongly recommend it.</p>
                                             
                                             "))),

                            tabItem(
                                tabName = 'neophyte',
                                box(
                                HTML('
<p><strong>This visualizations are by team Neophyte </strong></p>
                                     <p><strong>Dheepak Sreedharan , </strong></p>
                                     <p><strong>Venkatesan Thanulingam, </strong></p>
                                     <p><strong>Archana Durbhaka ,</strong></p>
                                     <p><strong>&nbsp;Sheik Abdul , </strong></p>
                                     <p><strong>&nbsp;Muthalip Abubakkar</strong></p>
                                     <p><strong>&nbsp;</strong></p>
                                     <p><strong>From University of Texas Dallas </strong></p>
                                     <p>&nbsp;</p>
                                     <ul>
                                     <li>First visualization is a dashboard representing overall information regarding the competition conducted by Rang Technologies. Out of 278 teams, 34% of teams have participated in public leader board. Out of 95 teams, almost 51% of teams have moved to Private leader board and among that 44% have been declared as winners!This visualization gives all the finalist information in one page, example number of attempts, university they belong and their scores in public and private leader boards.This visualization gives all the finalist information in one page, example number of attempts, university they belong and their scores in public and private leader boards.</li>
                                     </ul>
                                     <p><img src="visualization/neophyte/digital_metrics.png" alt="" width="400" height="200" /></p>
                                     <ul>
                                     <li>This visualization gives all the finalist information in one page, example number of attempts, university they belong and their scores in public and private leader boards.</li>
                                     </ul>
                                     <p><img src="visualization/neophyte/finalist_info.png" alt="" width="600" height="400" /></p>
                                     <ul>
                                     <li>This gives information regarding the number teams each university represents in the competition. It is seen that students at University of Texas Dallas have contributed for 19.5% of registrations for the competition followed by New York University.</li>
                                     </ul>
                                     <p><img src="visualization/neophyte/team_count.png" alt="" width="600" height="400" /></p>
                                     <ul>
                                     <li>This gives the analysis of top 10 teams and their comparison of scores and attempts. There is no proper correlation between score and attempt. However, an average attempt of 85 would have landed a team on toppers list from the graph.</li>
                                     </ul>
                                     <p><img src="visualization/neophyte/top_ten.png" alt="" width="600" height="400" /></p>
                                     <ul>
                                     <li>Let us analyze a little deeper on how the teams have been performing at each stage. The graph shows the increase in scores of team from public to private leaderboard. Among all teams (29) who had an increase in score, 14 teams (48%) have either won or are on toppers list. Remaining 15 have lost their game.</li>
                                     </ul>
                                     <p><img src="visualization/neophyte/positive_change.png" alt="" width="600" height="400" /></p>
                                     <ul>
                                     <li>Similarly, let us analyze the dip in scores of team from public to private leaderboard. Among all teams (19) who had a decrease in their scores, 7 teams (37%) have either won or are on toppers list. Remaining 12 have lost their game.</li>
                                     </ul>
                                     <p><img src="visualization/neophyte/negative_change.png" alt="" width="600" height="400" /></p>
                                     <p>Even though <strong>Columbia-ds</strong> had a dip in their score they were the final winners of the competition. But overall, it is seen that the teams whose score increased in private leader board had better chances of winning the game.</p>
                                     <p>&nbsp;</p>
                                     <ul>
                                     <li>Winners and Toppers: Rank trend analysis</li>
                                     </ul>
                                     <p><img src="visualization/neophyte/rank_trend.png" alt="" width="600" height="400" /></p>
                                     <p>Lesser the rank higher is the chance of winning the competition. It seen that almost all the teams (81%) rank is either reduced or remind constant. There may be many reasons for the reduction like increase in number of attempts, model accuracy and proper usage of time etc.</p>
                                     <p>&nbsp;</p>
                                     <ul>
                                     <li>It is observed that the submissions made on 1st and 2<sup>nd</sup> of June are higher even though the final submission due date was 5 June. And surprisingly teams who submitted in early week of June had higher winning probability.</li>
                                     </ul>
                                     <p><img src="visualization/neophyte/submission_count_june.png" alt="" width="600" height="400" /></p>                                                      
                                     '))
                            ),

                            tabItem(
                                tabName='jetty',
                                box(
                                    HTML('
                                         <p>&nbsp;&nbsp; <strong>Author : Rajasekhar Jetty&nbsp;&nbsp;&nbsp; University : University Of Texas at Dallas&nbsp;&nbsp;&nbsp;&nbsp;</strong></p>
<p><strong>&nbsp;&nbsp; Code link : &nbsp;&nbsp;<a href="https://github.com/raj2014/RangTechChallenge" target="_blank">https://github.com/raj2014/RangTechChallenge</a></strong></p>
                                         <p>&nbsp;</p>
                                         <ul>
                                         <li><strong>Private LeaderBoard Visualization</strong></li>
                                         </ul>
                                         <p><strong><img src="visualization/jetty/pvt_top_20.png" alt="" width="1200" height="800" /></strong></p>
                                         <p>&nbsp;</p>
                                         <ul>
                                         <li><strong>Rank shift from public to private LB</strong></li>
                                         </ul>
                                         <p><strong><img src="visualization/jetty/public_top_20.png" alt="" width="1200" height="800" /></strong></p>
                                         <p>&nbsp;</p>
                                         <ul>
                                         <li><strong>Impact of submissions on rank in private LB</strong></li>
                                         </ul>
                                         <p><strong><img src="visualization/jetty/sub_impact.png" alt="" width="1200" height="800" /></strong></p>
                                         <p>&nbsp;</p>
                                         <ul>
                                         <li><strong>Impact of TeamSize on PrivateLeaderBoard Rank- Top 10</strong></li>
                                         </ul>
                                         <p><strong><img src="visualization/jetty/team_size_impact.png" alt="" width="1200" height="800" /></strong></p>
                                         <p>&nbsp;</p>
                                         <ul>
                                         <li><strong>Toppers Composition : University Representation in Top20%</strong></li>
                                         </ul>
                                         <p><strong><img src="visualization/jetty/toppers_composition.png" alt="" width="1200" height="800" /></strong></p>
                                         ')
                                )
                            ),
                            
                            tabItem(
                                tabName = 'pvtLeaderboard',
                                p("Final submission from the Private Leaderboard, which determines the final winners or rankings of the competition."),
                                br(),
                                
                                dataTableOutput('pvtLbContent')
                            ),

                            tabItem(
                                tabName = 'pubLeaderboard',
                                p("This displays multiple attempts by participants and ranks the best attempt and number of attempts of the participant groups."),
                                br(),
                                dataTableOutput('pubLbContent')
                            ),
                            
                            tabItem(tabName='participants',
                                    dataTableOutput('allUsers')
                                    ),
                            tabItem(tabName='contact',
                                    HTML('
                                         <p><strong>Contact us</strong></p>
                                         <p>For password and id related problem send email to <a href="mailto:career@rangtech.com">career@rangtech.com</a> with subject line CASE PASSWORD and we will get back to you in 24 hours</p>
                                         <p>For any other case related problem send email to <a href="mailto:career@rangtech.com">career@rangtech.com</a> with subject line CASE and we will get back in 24 hours</p>
                                         <p>If you want to send your resume for any jobs send email to <a href="mailto:analytics.rangtech@jobdivaemail.com">analytics.rangtech@jobdivaemail.com</a> our team will contact you if there is any appropriate position available for you. Your resume will be stored in database and you may be contacted any time in future based on availability of appropriate position.</p>
                                         <p>&nbsp;</p>
                                         <p>Our address :</p>
                                         <p>Rang Technologies Inc, 15 Corporate Place (South), Suite 356, Piscataway, NJ, 08854. <a href="http://www.rangtech.com/" target="_blank">www.rangtech.com</a></p>
                                         <p>KVRA Tech Inc., 15 Corporate Place (South), Suite 369, Piscataway, NJ, 08854. <a href="http://www.kvratech.com/" target="_blank">www.kvratech.com</a></p>
                                         '))
                        )#End of tabItems
                        
                    )#end of dashboardBody
)#End of dashboardPage

#################### server #############################
server <- function(input, output,session) {
    #########  userInput ##############

    
#     output$userInput <- renderUI({
#         textInput(inputId = 'username',label = 'Group Name',value = NULL)        
#     })
#     
# 
#     output$passwordIntake <- renderUI({
#         passwordInput('password',label = 'Password')
#     })
#     
#     
#     output$submitIntake <- renderUI({
#         actionButton("submitbutton", "Submit")
#     })
# 
#     submission <- eventReactive(input$submitbutton,{
#         if(is.null(input$username) | is.null(input$password) | input$username=='' | input$password==''){
#             paste0("Please type Username and Password")
#         }
#         else{
#             un=input$username
#             pw=input$password
#             if(un %in% mydat$Group.Name){
#                 c <- as.character(mydat[mydat$Group.Name==un,]$Password)
#                 
#                 if (pw == c){
#                     is.logged$isLogged=TRUE
#                     assign(x='is.logged$isLogged',value=is.logged$isLogged,envir=.GlobalEnv)
#                     paste0("Hello ", un, " You have successfully logged in !")}
#                 else{
#                     paste0("Invalid Password")
#                 }
#             }
#             else{
#                 paste0("Invalid Username")
#             }
#             
#         }        
#     })
#     
#     output$submission<- renderText({
#         submission()
#     })    
#     
# 
#     
#         output$finSub <- renderUI({
#             #input$finalSubmit
#             #input$submit
#             if(is.logged$isLogged==FALSE)({
#                 shinyjs::disable('finalSubmit')
#                 return(h2("Please login"))
#             })
#             pvt_lb=drop_read_csv('scoreFiles/private_scoredata.csv',dtoken=token)
#             if(input$username %in% pvt_lb$Group.Name){
#                 userSub$alreadySubmitted=TRUE
#                 shinyjs::disable('finalSubmit')
#                 return(h2("You have already submitted your final submission"))
#             }
#             
#             scoredata2=drop_read_csv('scoreFiles/scoredata.csv',dtoken=token)
#             scoredata2=as.data.table(scoredata2)
#             if(!(input$username %in% scoredata2$Group.Name)){
#                 return(h2("You haven't made a single submission in public leaderboard"))
#             }
#             scoredata2=scoredata2[scoredata2$Group.Name==input$username,]
#             
#             radioButtons("scoreSubmission", label = h3("Select a final model from following public leaderboard scores"),
#                          choices=as.list(c("",as.character(unique(scoredata2$Score)))))
#         })
        
        #user interface for final submission
#         observeEvent(input$finalSubmit, {
#             #userSub$alreadySubmitted=TRUE
#             # User-experience stuff
#             shinyjs::disable("finalSubmit")
#             shinyjs::show("submit_msg")
#             shinyjs::hide("error")
#             
#             # Save the data (show an error message in case of error)
#             tryCatch({
#                 input$scoreSubmission
#                 
#                 shinyjs::hide("finSub")
#                 shinyjs::show("thankyou_msg")
#                 finalSub$made=TRUE
#             },
#             error = function(err) {
#                 shinyjs::html("error_msg", err$message)
#                 shinyjs::show(id = "error", anim = TRUE, animType = "fade")
#             },
#             finally = {
#                 shinyjs::enable("finalSubmit")
#                 shinyjs::hide("submit_msg")
#             })
#             
# 
#         })
        
        output$winnersTable <- renderDataTable({
            winnersList = read.csv("winners_n_toppers.csv",sep=',',header=TRUE)
            winnersList = as.data.table(winnersList)
            return(winnersList)
        },options=list(pageLength=50))
        output$pvtLbContent <- renderDataTable({
             
#             scoredata3=drop_read_csv('scoreFiles/scoredata.csv',dtoken=token)
#             scoredata3=as.data.table(scoredata3)
#             pvt_scoredata=drop_read_csv('scoreFiles/private_scoredata.csv',dtoken=token)
            
            pvt_scoredata=na.omit(pvt_scoredata)
            pvt_scoredata=as.data.table(pvt_scoredata)
            

#             if(is.logged$isLogged==FALSE){return(h2("Please Login"))}
#             else{
#                 
#              if(!(input$username %in% scoredata3$Group.Name)){
#                  return(h2("Your group hasn't made any submission in public leaderboard"))
#              }
#              if(#userSub$alreadySubmitted==TRUE |
#                  input$username %in% pvt_scoredata$Group.Name | finalSub$made==FALSE
#                  #| input$scoreSubmission==''
#                  ){
#                  output$table = renderDataTable(pvt_scoredata)
#                  dataTableOutput('table')
#              }
#             
#             else{
# 
#                 if(userSub$alreadySubmitted==FALSE & input$scoreSubmission!=''){
#             scoredata3=scoredata3[Group.Name==input$username & Score==as.numeric(input$scoreSubmission),]
#             name=input$username
#             attempts=max(scoredata3$Attempts)
#             testName= paste('scoreFiles/File',name,attempts,'.csv',sep='')
#         
#             userFile=drop_read_csv(testName,dtoken=token)
#             actual=finaldata[private,]$Active_Customer
#             pred = userFile[private,]$Active_Customer
#             ttt = confusionMatrix(data=pred,reference = actual,dnn=c('Prediction','Actual'),prevalence = 1)
#             accuracy = as.character(round(ttt$overall['Accuracy'],6))
#             pvt_scoredata=rbindlist(list(pvt_scoredata,data.table(Group.Name=input$username,University=scoredata3$University[1],Score=accuracy,Time=as.character(with_tz(Sys.time(),'EST')))))
#             #pvt_scoredata=rbindlist(list(pvt_scoredata,data.table(Group.Name=scoredata2$Group.Name,University=scoredata2$University,Score=accuracy,Time=format(Sys.time(),"%a %b %d %X %Y"))))
#             pvt_scoredata=pvt_scoredata[order(Score,decreasing=T)]
#             pvt_scoredata=na.omit(pvt_scoredata)
#             write.csv(pvt_scoredata,'private_scoredata.csv',row.names=F,na="")
#     
#             drop_upload('private_scoredata.csv',dest=filePath,dtoken=token)
#             
#             output$table2 = renderDataTable(pvt_scoredata)
#             dataTableOutput('table2')
#                 }
#             }
#                 }
            
            
        },options=list(pageLength=100))
        
        output$pubLbContent <- renderDataTable({
            pubLb = data.table(public_scoredata)
            return(pubLb)
        },options=list(pageLength=100))
        
        output$allUsers <- renderDataTable({
            #if(is.logged$isLogged==FALSE){return(NULL)}
            #mydat = drop_read_csv('scoreFiles/mydat.csv',dtoken = token)
            mydat2=data.table('Group Name'=mydat$Group.Name,
                              University=mydat$University.Name,
                              'Participant 1'=mydat$Participant_1,
                              'Participant 2'=mydat$Participant_2,
                              'Participant 3'=mydat$Participant_3,
                              'Participant 4'=mydat$Participant_4,
                              'Participant 5'=mydat$Participant_5)
            return(mydat2)
        },options=list(pageLength=100))
    
}

shinyApp(ui, server)