library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)

ui <- dashboardPage( title = "Stream Predictor", skin = "green",
    dashboardHeader(
    title = "Stream Predictor",
    dropdownMenu(type = "message",
                 messageItem(from = "Update",message = "we are on threshold"),
                 messageItem(from="Data Update",message = "Data is updated in Database",icon=icon("database"),time="22:00"),
                 messageItem(from="Tasks Update",message = "Currently A Tasks is Running",time="15:00",icon = icon("spinner"))
                 
    
),dropdownMenu(type="notifications",
               notificationItem(
                   text="2 new tabs added to the dashboard",
                   icon = icon("dashboard"),
                   status="success"
               ),
               notificationItem(text="server is currently running at 80% load",
                                icon = icon("warning"),
                                status = "warning"
               )  
),

dropdownMenu(type="tasks",
             taskItem(
                 value=80,
                 color = "red",
                 "Tasks Running"
             ),
             
             taskItem(
                 value = 70,
                 color = "blue",
                 "Health Status"
             )
             
)
),
   
       
   
dashboardSidebar(
    sidebarMenu( sidebarSearchForm("Search text","buttonSearch","Search"),
    
    menuItem(text = "Stream Predictor", tabName = "main",icon = icon("file-excel-o")  ),
    menuItem("Progress Report", tabName = "dashboard",icon =icon("tasks")),
    menuItem("View Data",tabName = "data",icon=icon("database"))
    
    )
),


dashboardBody(
    fluidPage(
        tabItems(
            
            tabItem(tabName = "main",
                    fluidRow(
                        
                        valueBox(44,"Subjects",icon=icon("list"),color = "red"),
                        valueBox(25,"Gate Subjects",icon=icon("list-ol"),color = "green"),
                        valueBox(8,"Semesters",icon=icon("list-ul"),color = "blue"),
                        
                        box(width = 12,
                            fileInput(inputId = "file",
                                      label = "Choose a file",
                                      accept = c(".xlsx",".csv")
                            ),
                           
                            verbatimTextOutput("Data")
                        )
                    )
            ),
            tabItem(tabName = "dashboard",
                    fluidRow(
                        box(width = 12,
                            fileInput(inputId = "file1",
                                      label = "Choose a file",
                                      accept = c(".xlsx",".csv")
                            ),
                            verbatimTextOutput("progress"),
                            plotOutput("progressSGPA"),
                            plotOutput("progressCGPA")
                            
                        )
                    ))
        )
    )
))



server <- function(input, output, session) {
    output$Data <- renderPrint({
        if(is.null(input$file)){
            print("Import The Data in CSV Format")
        } else {
            inFile <- input$file
            df <- read.csv(inFile$datapath)
            print(df)
            
            
            
            
            programming <- df %>% filter(Subjects=="IP" | Subjects=="ESFP-I"|Subjects=="FP" | Subjects=="OOP"| Subjects=="ESFP-II"| Subjects=="BOSS" | Subjects=="DBMS" | Subjects=="DS" | Subjects=="OS" | Subjects=="WT" | Subjects=="TC" | Subjects=="AD")
            pr_avg_th <- mean(programming$Theory)
            pr_avg_lb <- mean(programming$Lab)
            print("The Mean Marks of all Programming Subject in Theory is : ")
            print(pr_avg_th )
            print("The Mean Marks of all Programming Subject in Lab is : ")
            print(pr_avg_lb )
            
            gate <-  df %>% filter(Subjects=="Calculus" |Subjects=="DE"|Subjects=="BE" |Subjects=="MNM"|Subjects=="PNS" |Subjects=="AEM" |Subjects=="BCS"|Subjects=="CO" |Subjects=="ASB")
            g_avg_th <- mean(gate$Theory)
            g_avg_lb <- mean(gate$Lab)
            br()
            print("The Mean Marks of all Gate Subject in Theory is : ")
            print(g_avg_th )
            print("The Mean Marks of all Gate Subject in Lab is : ")
            print(g_avg_lb )
            
            networking <- df %>% filter(Subjects=="Calculus" |Subjects=="DE"|Subjects=="BE" |Subjects=="MNM"|Subjects=="BCS"|Subjects=="CO" )
            nt_avg_th <- mean(networking$Theory)
            nt_avg_lb <- mean(networking$Lab)
            print("The Mean Marks of all Networking Subject in Theory is : ")
            print(nt_avg_th )
            print("The Mean Marks of all Networking Subject in Theory is : ")
            print(nt_avg_lb )
            
            if(pr_avg_th>=g_avg_th & pr_avg_lb>=g_avg_lb & pr_avg_th>=nt_avg_th & pr_avg_lb>=nt_avg_lb  )
            {
                print("Your programming is good,you should opt for web development")
            }
            else if(g_avg_th>=pr_avg_th & g_avg_lb>=pr_avg_lb & g_avg_th>=nt_avg_th & g_avg_lb>=nt_avg_lb)
            {
                print("You Could opt for Gate")
            }
            else if(nt_avg_th>=g_avg_th & nt_avg_lb>=g_avg_lb & nt_avg_th>=pr_avg_th & nt_avg_lb>=pr_avg_lb)
            {
                print("Your networking subjects are good ,you should opt for network related area")
            }else{
                print("you should try harder and keep exploring new domains to suit your interest.")
            }
            
          
            
        }
    })
    
    output$progress <- renderPrint({
        if(is.null(input$file1)){
            print("Import CSV data file")
        } else {
            inFile <- input$file1
            df <- read.csv(inFile$datapath)
            print(df)
            
            }
        
    })
   
    
    output$progressCGPA <- renderPlot({
        inFile <- input$file1
        cgpa <- read.csv(inFile$datapath)
        #print("Graph of CGPA per sem")
        ggplot(cgpa, aes(x = cgpa$Sem, y = cgpa$CGPA))+geom_line() + theme_wsj()+labs(x="Sem",y="CGPA")
        
        
    })
    
    output$progressSGPA <- renderPlot({
        inFile <- input$file1
        sgpa <- read.csv(inFile$datapath)
        #print("Graph of SGPA per sem")
        #plot(x$Sem,x$SGPA, colour="blue")
        ggplot(sgpa, aes(y = sgpa$SGPA, x = sgpa$Sem)) + geom_violin(color="green") + geom_point(color="red")+ labs(x="Sem",y="SGPA") 
    })
    
    output$dtable <- renderTable({
        inFile <- input$file1
        sgpa <- read.csv(inFile$datapath)
        sgpa
    })
   
}
shinyApp(ui = ui, server = server)

