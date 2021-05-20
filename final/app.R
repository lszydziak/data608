library(ggplot2)
library(dplyr)
library(plotly)
library(readr)
library(shiny)

covid<-"https://raw.githubusercontent.com/lszydziak/data608/main/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries_043021.csv"

##covid<-"C:/Users/Lisa/Documents/CUNY/608/final/finaluse/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries_043021.csv"

covid1<-read.table(file=covid,header=TRUE, sep=",")

covid1a<-select(covid1,state,date,inpatient_beds_used_covid,total_adult_patients_hospitalized_confirmed_and_suspected_covid, total_pediatric_patients_hospitalized_confirmed_and_suspected_covid,staffed_icu_adult_patients_confirmed_and_suspected_covid,inpatient_beds_utilization, adult_icu_bed_utilization)

colnames(covid1a)<-c("State","Date2","IP_Covid_Occ","Tot_Covid_Adult_IP","Total_Covid_Ped_IP", "Tot_Covid_ICU","IP_Util","ICU_Util")

class(covid1a)
#Take a peek at the first few records
head(covid1a)
# What is the size of this dataset?
dim(covid1a)

#What types of variables are in the dataset?
str(covid1a)

covid1a<-covid1a  %>% mutate(IP_Availability=1-IP_Util)


covid1a<-covid1a  %>% mutate(ICU_Availability=1-ICU_Util)



covid1a$Date1<-as.Date(covid1a$Date2,format="%m/%d/%Y")  
covid1a$Date<-as.character(covid1a$Date1) 
covid_all<-covid1a

covid1a<-covid_all %>%
  select(State,Date2,IP_Covid_Occ,Tot_Covid_Adult_IP,Total_Covid_Ped_IP,Tot_Covid_ICU,IP_Util,ICU_Util,IP_Availability,ICU_Availability,Date1,Date) %>%
 filter(Date1>"2020-03-27")

covid2a<-covid_all %>%
  select(State,Date2,IP_Covid_Occ,Tot_Covid_Adult_IP,Total_Covid_Ped_IP,Tot_Covid_ICU,IP_Util,ICU_Util,IP_Availability,ICU_Availability,Date1,Date) %>%
  filter(Date1>"2020-03-27")

covid2a<-covid2a %>% select(State,Date2,IP_Covid_Occ,Tot_Covid_Adult_IP,Total_Covid_Ped_IP,Tot_Covid_ICU,IP_Util,ICU_Util,IP_Availability,ICU_Availability,Date1,Date) %>%
                arrange(State)

covid_graph<-plot_geo(covid1a,locationmode='USA-states', frame=  ~Date) %>%
  add_trace(locations= ~State,
            z= ~IP_Covid_Occ,
            zmin = 0,
            #zmax = max(covid1a$IP_Covid_Occ,as.rm=TRUE),
            color= ~IP_Covid_Occ, 
            colorscale='Electic') %>%
  
  layout(geo=list(scope='usa'),
         title="Suspected/Confirmed Covid-19 Hospital cases\nMarch 28,2020 - April 30, 2021") %>%
  config(displayModeBar=FALSE) 

##covid_graph <- covid_graph %>% layout(legend=list(title=list(text='<b> Trend </b>')))

##########################


#quick summary
#summary(CDC)c

#What are the variables?
#colnames(CDC)
#########



ui <- fluidPage(tabsetPanel(
  tabPanel(title="Covid-19 Reported Patient Impact by State",
           headerPanel("USA:  Time Lapse Covid-19 Hospitalizations"),
           sidebarPanel(
             h3("Dataset Background"),br(),
             p("State-aggregated data for hospital utilization in a timeseries format. Facility-level granularity across three main sources: (1) HHS TeleTracking, (2) reporting provided directly to HHS Protect by state/territorial health departments on behalf of their healthcare facilities and (3) National Healthcare Safety Network (before July 15)."),
             br(),p(style="font-family:Impact",
               "Link to website:  ",
               a("COVID-19 Reported Patient Impact and Hospital Capacity by State Timeseries | HealthData.gov",
                 href="https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh")
               )),
             
           mainPanel(
             covid_graph)
           
  ),
  
  tabPanel(title="By State:  Covid-19 Cases / Hospital Availablity",
           headerPanel('State-specific Covid Cases: Bed Availability'),
           fluidRow(
             column(6, h4("Inpatient or ICU Bed"),selectInput(inputId="Level2", 
                      label="Choose a IP or ICU stay", 
                      c("IP","ICU"), selected=''),
                      selectInput(inputId="State2", 
                      label="Choose a State", 
                      unique(covid2a$State), selected='')
                    ),
             column(6,br(),p("Monitoring number of hospitalizations in conjunction with bed availability aids in government's decisions regarding increasing bed capacity and shutdowns."),br(),p("Note: Time period: 3/28/20 - 4/20/21.  ICU data available after 7/15/20, curve smoothing applied."))),
             
           fluidRow(
             column(6,plotOutput(outputId = "plot2")),
             column(6,plotOutput(outputId = "plot3"))
        ))),)     
           
           
  



server <- function(input, output) {
  
  
  dfSlice <- reactive({(covid2a %>%
                        filter(State == input$State2))})
  
   Level<-reactive({input$Level2}) 
   
   
    output$plot2 <- renderPlot({
    
   
      if (Level()=="IP")
        
      {ggplot(dfSlice(), aes(x = Date1, y= IP_Covid_Occ)) +  geom_line(color = "#00AFBB", size = 2)+labs(title="Inpatient covid cases",x="",
                                                                                                          y = "Inpatient Suspected/Confirmed cases")
      } else
      
      if (Level()=="ICU")
        
      {ggplot(dfSlice(), aes(x = Date1, y= Tot_Covid_ICU)) +  geom_line(color = "#00AFBB", size = 2)+labs(title="ICU patient covid cases",x="",
                                                                                                           y = "ICU Suspected/Confirmed cases")
      }
      })
    
    output$plot3 <- renderPlot({
      
      if (Level()=="IP")
      
      {ggplot(dfSlice(), aes(x = Date1, y= IP_Availability)) +  geom_line(color = "#FC4E07")+ stat_smooth(
        color = "#FC4E07", fill = "#FC4E07",
        method = "loess")+scale_y_continuous(limits = c(.1, .6))+labs(title="Inpatient Availability",x="",
                                                                       y = "1-Inpatient Utilization")}
      
      else if (Level()=="ICU")
        
        {ggplot(dfSlice(), aes(x = Date1, y= ICU_Availability)) +  geom_line(color = "#FC4E07")+ stat_smooth(
          color = "#FC4E07", fill = "#FC4E07",
          method = "loess")+scale_y_continuous(limits = c(.2, .6))+labs(title="ICU Availability",x="",
                                                                         y = "1-ICU Utilization")}})
            
}        
      
  
  


shinyApp(ui = ui, server = server)
