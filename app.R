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
library(ggplot2)
#library(plotly)
#library(ggthemes)
#library(googlesheets)
library(lubridate)
library(DT)
library(caret)
library(e1071)
#require(RCurl)

#myCsv=getURL("https://docs.google.com/spreadsheets/d/1fafcphCY6JOgiQXiANQVw_mEKmOKyM9EscUp5pib53I/pub?output=csv")

token <- readRDS("droptoken.rds")
mydat=drop_read_csv('scoreFiles/mydat.csv',dtoken=token)

is.logged <-reactiveValues(isLogged=FALSE)
userSub <- reactiveValues(alreadySubmitted=FALSE)
finalSub <- reactiveValues(made=FALSE)
################# ui ########################
ui <- dashboardPage(skin='purple',
                    dashboardHeader(title='Rang Technologies Competition'),
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Instructions", tabName='instructions', icon=icon('first-order')),
                            menuItem("Login",tabName='loggin', icon = icon("hand-pointer-o")#,
                                     #selectInput('username','Group Name', choices=as.list(as.character(mydat$Group.Name))),
                                     #fileInput('file1','File',accept=c('text/csv','.csv'))
                            ),
                            menuItem(text = 'Final Submission', tabName='finalSubmission',icon=icon('skyatlas')),
                            menuItem(text='Private Leaderboard',tabName='pvtLeaderboard',icon=icon('trophy')),
                            menuItem(text='Participants', tabName='participants',icon=icon('users')),
                            menuItem('Contact Us', tabName='contact', icon=icon('at'))
                        )#End of dabhboard sidebarMenu
                    ), #end of dashboardSidebar
                    dashboardBody(
                        tabItems(
                            tabItem(tabName='instructions',
                                    box(title='Instructions', width=10,
                                        HTML("
                                             <p>
1. Login with username and password
                                             </p>
                                             
                                             <p>
                                             2. Select a score for final submission. Please wait till scores appear and click on 'Make Final Submission' button. If you already made final submission you can skip this step and check how your submission ranks against others in private leaderboard tab
                                             </p>
                                             
                                             <p>
                                             3. See how you rank against others in private leaderboard. Please wait till leaderboard appears
                                             </p>
                                             
                                             
                                             If you face any latency issues refresh browser or close tab and try logging in again.
                                             If you still face issues send us an email to <a href='mailto:career@rangtech.com'>career@rangtech.com</a> with subject line CASE and we will get back in 24 hours
                                             
                                             "))
                                    ),
                            tabItem(tabName='loggin',
                                    uiOutput('userInput'),
                                    uiOutput('passwordIntake'),
                                    uiOutput('submitIntake'),
                                    p("If you forget your password, send us an email at career@rangtech.com with subject CASE PASSWORD and we will get back to you in 24 hours!"),
                                    hr(),
                                    verbatimTextOutput('submission')
                            ),

                            
                            tabItem(tabName = 'finalSubmission',
                                    shinyjs::useShinyjs(),
                                    h3("Please wait till scores appear!"),
                                    uiOutput('finSub'),
                                    actionButton(inputId = 'finalSubmit',label='Make Final Submission',icon=icon('check')),
                                    shinyjs::hidden(
                                        span(id = "submit_msg", "Submitting..."),
                                        div(id = "error",
                                            div(br(), tags$b("Error: "), span(id = "error_msg"))
                                        )
                                    ),
                                    shinyjs::hidden(
                                        div(
                                            id = "thankyou_msg",
                                            h3("Thank you, You have made your final submission!")
                                            #actionLink("submit_another", "Submit another response")
                                        )
                                    )
                                    ),
                            
                            tabItem(
                                tabName = 'pvtLeaderboard',
                                h3(
                                    "Please login before viewing private leaderboard"
                                ),
                                uiOutput('pvtLbContent')
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
)

#################### server #############################
server <- function(input, output,session) {
    #########  userInput ##############

    
    output$userInput <- renderUI({
        textInput(inputId = 'username',label = 'Group Name',value = NULL)        
    })
    

    output$passwordIntake <- renderUI({
        passwordInput('password',label = 'Password')
    })
    
    
    output$submitIntake <- renderUI({
        actionButton("submitbutton", "Submit")
    })

    submission <- eventReactive(input$submitbutton,{
        if(is.null(input$username) | is.null(input$password) | input$username=='' | input$password==''){
            paste0("Please type Username and Password")
        }
        else{
            un=input$username
            pw=input$password
            if(un %in% mydat$Group.Name){
                c <- as.character(mydat[mydat$Group.Name==un,]$Password)
                
                if (pw == c){
                    is.logged$isLogged=TRUE
                    assign(x='is.logged$isLogged',value=is.logged$isLogged,envir=.GlobalEnv)
                    paste0("Hello ", un, " You have successfully logged in !")}
                else{
                    paste0("Invalid Password")
                }
            }
            else{
                paste0("Invalid Username")
            }
            
        }        
    })
    
    output$submission<- renderText({
        submission()
    })    
    

    
        output$finSub <- renderUI({
            #input$finalSubmit
            #input$submit
            if(is.logged$isLogged==FALSE)({
                shinyjs::disable('finalSubmit')
                return(h2("Please login"))
            })
            pvt_lb=drop_read_csv('scoreFiles/private_scoredata.csv',dtoken=token)
            if(input$username %in% pvt_lb$Group.Name){
                userSub$alreadySubmitted=TRUE
                shinyjs::disable('finalSubmit')
                return(h2("You have already submitted your final submission"))
            }
            
            scoredata2=drop_read_csv('scoreFiles/scoredata.csv',dtoken=token)
            scoredata2=as.data.table(scoredata2)
            if(!(input$username %in% scoredata2$Group.Name)){
                return(h2("You haven't made a single submission in public leaderboard"))
            }
            scoredata2=scoredata2[scoredata2$Group.Name==input$username,]
            
            radioButtons("scoreSubmission", label = h3("Select a final model from following public leaderboard scores"),
                         choices=as.list(c("",as.character(unique(scoredata2$Score)))))
        })
        
        #user interface for final submission
        observeEvent(input$finalSubmit, {
            #userSub$alreadySubmitted=TRUE
            # User-experience stuff
            shinyjs::disable("finalSubmit")
            shinyjs::show("submit_msg")
            shinyjs::hide("error")
            
            # Save the data (show an error message in case of error)
            tryCatch({
                input$scoreSubmission
                
                shinyjs::hide("finSub")
                shinyjs::show("thankyou_msg")
                finalSub$made=TRUE
            },
            error = function(err) {
                shinyjs::html("error_msg", err$message)
                shinyjs::show(id = "error", anim = TRUE, animType = "fade")
            },
            finally = {
                shinyjs::enable("finalSubmit")
                shinyjs::hide("submit_msg")
            })
            

        })
        output$pvtLbContent <- renderUI({
             
            scoredata3=drop_read_csv('scoreFiles/scoredata.csv',dtoken=token)
            scoredata3=as.data.table(scoredata3)
            pvt_scoredata=drop_read_csv('scoreFiles/private_scoredata.csv',dtoken=token)
            pvt_scoredata=na.omit(pvt_scoredata)
            

            if(is.logged$isLogged==FALSE){return(h2("Please Login"))}
            else{
                
             if(!(input$username %in% scoredata3$Group.Name)){
                 return(h2("Your group hasn't made any submission in public leaderboard"))
             }
             if(#userSub$alreadySubmitted==TRUE |
                 input$username %in% pvt_scoredata$Group.Name | finalSub$made==FALSE
                 #| input$scoreSubmission==''
                 ){
                 output$table = renderDataTable(pvt_scoredata)
                 dataTableOutput('table')
             }
            
            else{

                if(userSub$alreadySubmitted==FALSE & input$scoreSubmission!=''){
            scoredata3=scoredata3[Group.Name==input$username & Score==as.numeric(input$scoreSubmission),]
            name=input$username
            attempts=max(scoredata3$Attempts)
            testName= paste('scoreFiles/File',name,attempts,'.csv',sep='')
        
            userFile=drop_read_csv(testName,dtoken=token)
            actual=finaldata[private,]$Active_Customer
            pred = userFile[private,]$Active_Customer
            ttt = confusionMatrix(data=pred,reference = actual,dnn=c('Prediction','Actual'),prevalence = 1)
            accuracy = as.character(round(ttt$overall['Accuracy'],6))
            pvt_scoredata=rbindlist(list(pvt_scoredata,data.table(Group.Name=input$username,University=scoredata3$University[1],Score=accuracy,Time=as.character(with_tz(Sys.time(),'EST')))))
            #pvt_scoredata=rbindlist(list(pvt_scoredata,data.table(Group.Name=scoredata2$Group.Name,University=scoredata2$University,Score=accuracy,Time=format(Sys.time(),"%a %b %d %X %Y"))))
            pvt_scoredata=pvt_scoredata[order(Score,decreasing=T)]
            pvt_scoredata=na.omit(pvt_scoredata)
            write.csv(pvt_scoredata,'private_scoredata.csv',row.names=F,na="")
    
            drop_upload('private_scoredata.csv',dest=filePath,dtoken=token)
            
            output$table2 = renderDataTable(pvt_scoredata)
            dataTableOutput('table2')
                }
            }
                }
            
            
        })
        
        output$allUsers <- renderDataTable({
            if(is.logged$isLogged==FALSE){return(NULL)}
            mydat = drop_read_csv('scoreFiles/mydat.csv',dtoken = token)
            mydat2=data.table('Group Name'=mydat$Group.Name,
                              University=mydat$University.Name,
                              'Participant 1'=mydat$Participant_1,
                              'Participant 2'=mydat$Participant_2,
                              'Participant 3'=mydat$Participant_3,
                              'Participant 4'=mydat$Participant_4,
                              'Participant 5'=mydat$Participant_5)
            return(mydat2)
        })
    
}

shinyApp(ui, server)