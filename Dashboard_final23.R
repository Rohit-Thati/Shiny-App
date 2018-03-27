library("shinydashboard")
library("shiny")
library("RMySQL")
library("RSQLite")
library("ggplot2")
library("sqldf")
library("DT")
library("shinythemes")
library("lubridate")
library("plotly")
library("leaflet")
library("geojson")
library("geojsonio")
library("sp")
library("maptools")
library("reshape")
library("shinyjs")
library("data.table")
library("rCharts")
library("rjson")
library("shinyBS")

#Appstatus
mysqlconnection = dbConnect(MySQL(), user = 'thatirohit', password = 'thatirohit123', dbname = 'decision',
                            host = 'reports-rds.adfdata.net')
#options("scipen"=100, "digits"=4)
x= dbSendQuery(mysqlconnection,"select temp.*,round((drp*100)/applications,2) Drop_Percent,round((Esign_Withdrawns*100)/applications,2) Esign_Withdraw_Percent, round((autodecl*100)/applications,2) auto_Decline_Percent, round((OverallPended*100)/applications,2) Pended_Percent,
               round((autoapprvd*100)/applications,2) Auto_Approved_Percent, round((manual_apr*100)/applications,2) ManualApprvd_Percent,round((manual_apr*100)/OverallPended,2) Manual_apr_Percent_pend_percent,
               round((manual_decl*100)/OverallPended,2) Manual_decl_Percent_pend_percent,
               round((Esigned*100)/(autoapprvd+manual_apr),2) Esign_Percent, round((Funded*100)/Esigned,2) Funded_percent
               from (
               select  date_format(Date(app.CreateDateTime),'%m/%d') Dat,count(distinct case when app.ApplicationID is not null then app.ApplicationID end) applications,
               count(distinct case when app.ApplicationID is not null and (app.AppStatus in ('DROPPED','INPROGRESS') or app.AppStatus is null) then app.ApplicationID end) drp,
               
               count(distinct case when app.ApplicationID is not null and app.appstatus='AUTO_REJECT'  then app.ApplicationID end) autodecl,
               count(distinct case when app.ApplicationID is not null and  app.appstatus = 'AUTO_APPROVE'
               then app.ApplicationID end) autoapprvd,
               
               count(distinct case when app.ApplicationID is not null and app.AppStatus = 'RC_REVIEW' then app.ApplicationID end)RC_REVIEW,
               count(distinct case when app.ApplicationID is not null and app.AppStatus = 'RC_ELIGIBLE' then app.ApplicationID end)RC_ELIGIBLE,
               count(distinct case when app.ApplicationID is not null and app.disposition is not null then app.ApplicationID end)OverallPended,
               
               count(distinct case when app.ApplicationID is not null and app.AppStatus = 'PEND' then app.ApplicationID end)Curr_Pend,
               
               
               
               count(distinct case when app.ApplicationID is not null and  (app.appstatus='MANUAL_REJECT') then app.ApplicationID end)manual_decl,
               count(distinct case when app.ApplicationID is not null and (app.appstatus = 'MANUAL_APPROVE' ) then app.ApplicationID end)manual_apr,
               count(distinct case when app.ApplicationID is not null and isapproved = 0 and app.AppStatus = 'WITHDRAWN' then app.ApplicationID end)Pend_Withdrawns,
               
               count(distinct case when app.ApplicationID is not null and isapproved = 1 and app.AppStatus = 'WITHDRAWN'  then app.ApplicationID end)Esign_Withdrawns,
               count(distinct case when app.ApplicationID is not null and ln.loanid is not null and (app.Appstatus = 'APPROVE' or app.appstatus = 'MANUAL_APPROVE' or app.Appstatus = 'Approved'
               or app.appstatus = 'AUTO_APPROVE') and e.LoanID is not null
               then e.LoanID   end)Esigned, count(distinct case when ln.loanid is not null and cl.loan__Loan_Status__c not in('Approved','Canceled','Partial Application','Pending Approval')
               and (app.Appstatus = 'APPROVE' or
               app.Appstatus = 'Approved'  or app.appstatus = 'AUTO_APPROVE' or app.appstatus = 'MANUAL_APPROVE') then cl.Id end)Funded, concat('$', Format( sum(case when concat(ln.loanid,' ',cl.loan__Loan_Amount__c) is not null
               and cl.loan__Loan_Status__c
               not in('Approved','Canceled','Partial Application','Pending Approval') then cl.loan__Loan_Amount__c end),0)) Amount from application app
               left join   loan ln on app.ApplicationID=ln.Application_ApplicationID left join
               Esign e on ln.LoanID=e.LoanID left join   (select cl.name,cl.loan__Loan_Status__c,cl.id ,cl.loan__Loan_Amount__c,cd.loan__Sent_To_ACH__c 'normal',ca.loan__Sent_To_ACH__c 'feb',ca.loan__Reversed__c 'reversenormal',cd.loan__Reversed__c 'reversefeb',loan__Rejected__c from cl_import.loan__Loan_Account__c cl join cl_import.loan__Loan_Disbursal_Transaction__c cd on cl.id=cd.loan__Loan_Account__c
               left join cl_import.loan__Disbursal_Txn_Distribution__c ca on cd.id=ca.loan__Loan_Disbursal_Transaction__c where
               date(cl.CreatedDate) >= date(date_sub(CONVERT_TZ(current_timestamp(),'UTC','US/Pacific'), interval 3 day)) having( (cd.loan__Sent_To_ACH__c='1' or ca.loan__Sent_To_ACH__c='1') and (cd.loan__Reversed__c!='1' or ca.loan__Reversed__c!=1) and  loan__Rejected__c!=1)) cl on ln.sf_loan_number=cl.name where date(app.CreateDateTime) =
               date(date_sub(CONVERT_TZ(current_timestamp(),'UTC','US/Pacific'), interval 1 day)) and app.applicationid not in (select temp1.applicationid from(
               select (ap1.applicationid),count( case when  ap1.applicationid in (select rcl.new_appl_id from decision.application app1
               join decision.rate_correction_loan rcl on
               app1.applicationid=rcl.old_appl_id) then 1  end) applications
               from application ap1 group by 1 having applications=1)temp1))temp;")
y= fetch(x, n = 1000)
#print(y)



#loan count
u=dbSendQuery(mysqlconnection,"select count(distinct sf_loan_number) Number from loan;")
u1=fetch(u,n=100)
#print (u1)


#application date
yt=dbSendQuery(mysqlconnection,"
               select date(CreateDateTime) as Date, count(distinct ApplicationID) 'Applications',
               count(case when AppStatus LIKE '%APPROVE%' then applicationid end) 'Approvals',
               count(case when AppStatus LIKE '%REJECT%' then applicationid end) 'Rejects',
               count(case when AppStatus='DROPPED' then applicationid end) 'DROPPED',
               count(case when AppStatus like '%pend%' then applicationid end)'Pend',
               count(case when AppStatus='WITHDRAWN' then applicationid end) 'WITHDRAWN'
               from application
               where CreateDateTime>date_sub(convert_tz(now(), 'UTC','US/PACIFIC'), interval 5 day) group by 1")
yt1=fetch(yt,n=100)
#print (yt1)
yt17<-melt(yt1)
yt17$Date<-as.Date(yt17$Date)
names(yt17)<-c("Date","status","value")

options(scipen = 999)

ze<-dbSendQuery(mysqlconnection,"select count(*) from loan where inactive=0")
ze1=fetch(ze,n=10)
#print(ze1)


ze5<-dbSendQuery(mysqlconnection,"select count(*) from loan where inactive=1")
ze4=fetch(ze5,n=10)
#print(ze4)

result = dbSendQuery(mysqlconnection,"SELECT  DATE(max(l.created_at)) Time,
                     tb.LeadID,
                     l.Channel,
                     CASE when tb.VL LIKE '%NOTINSTATEREJECTED%' then 'Unsupported States' WHEN c.state is not null then c.state when d.state is not null then d.state else 'Others' end State,
                     COUNT(DISTINCT tb.transactionid) LP,
                     COUNT(DISTINCT CASE WHEN tb.vl='30DAYSREJECTED' OR tb.vl='NOTINSTATEREJECTED' THEN tb.transactionid END) DDP,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%EMAILVERIFICATIONSTARTED%' THEN tb.transactionid END) E1,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%EMAILVERIFICATIONSUBMITTED%' THEN tb.transactionid END) E2,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%EMAILVERIFIED%' THEN tb.transactionid END) E3,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%APPLICATIONSTARTED%' THEN tb.transactionid END) A,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%APPLICATIONSTARTED%' AND tb.vl NOT LIKE '%APPLICATIONSUBMITTED%' AND tb.vl NOT LIKE '%DEREJECTED%'
                     AND tb.vl NOT LIKE '%DEFAULTERREJECTED%' AND tb.vl NOT LIKE '%30DAYSREJECTED%' AND tb.vl NOT LIKE '%PAYDAYLOANREJECTED%'
                     AND tb.vl NOT LIKE '%
                     EXISTINGLOANREJECTED%' AND tb.vl NOT LIKE '%NOTINSTATEREJECTED%' AND NOT tb.vl LIKE '%DEFAULTERORPAYDAYLOANREJECTED%' THEN tb.transactionid END) Drp,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%APPLICATIONSTARTED%' AND tb.vl NOT LIKE '%APPLICATIONSUBMITTED%'
                     AND (tb.vl LIKE '%DEREJECTED%' OR tb.vl LIKE '%DEFAULTERREJECTED%' OR tb.vl LIKE '%30DAYSREJECTED%' OR tb.vl LIKE '%PAYDAYLOANREJECTED%'
                     OR tb.vl LIKE '%EXISTINGLOANREJECTED%' OR tb.vl LIKE '%NOTINSTATEREJECTED%' OR tb.vl LIKE '%DEFAULTERORPAYDAYLOANREJECTED%') THEN tb.transactionid END) Dcl,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%LOANDESIGNSTARTED%' THEN tb.transactionid END) LD,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%LOANDESIGNSTARTED%' AND tb.vl NOT LIKE '%LOANDESIGNSUBMITTED%' THEN tb.transactionid END) Drp,
                     
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%BANKDATASTARTED%' THEN tb.transactionid END) BD,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%BANKDATASTARTED%' AND tb.vl NOT LIKE '%DLVERIFICATION%' THEN tb.transactionid END) Drp,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%DLVERIFICATIONSKIPPED%' THEN tb.transactionid END) DLSkp,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%DLVERIFICATIONSTARTED%' THEN tb.transactionid END) DL,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%DLVERIFICATIONSTARTED%' AND tb.vl NOT LIKE '%REPAYMENTSTARTED%' THEN tb.transactionid END) Drp,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%REPAYMENTSTARTED%' THEN tb.transactionid end) RP,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%REPAYMENTSTARTED%' AND (tb.vl NOT LIKE '%REPAYMENTSUBMITTED%' AND tb.vl NOT LIKE '%APPROVED%'
                     AND tb.vl NOT LIKE '%DEREJECTED%' AND tb.vl NOT LIKE '%DESOFTREJECTED%' AND tb.vl NOT LIKE '%HRFAPEND%') THEN tb.transactionid END) Drp,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%REPAYMENTSTARTED%' AND tb.vl LIKE '%DEREJECTED%' THEN tb.transactionid END) Dcl,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%APPROVED%' AND tb.vl NOT LIKE '%DESOFTREJECTED%' THEN tb.transactionid END) AA,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%DESOFTREJECTED%' AND tb.vl NOT LIKE '%APPROVED%' AND da.appstatus LIKE 'PEND' THEN tb.transactionid END) CP,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%BANKDATASTARTED%' AND tb.vl LIKE '%DESOFTREJECTED%' AND da.appstatus LIKE 'REJECT' THEN tb.transactionid END) MD,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%DESOFTREJECTED%' AND (tb.vl LIKE '%APPROVED%' OR da.appstatus = 'MANUAL_APPROVE') THEN tb.transactionid END) MA,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%DESOFTREJECTED%' AND da.appstatus='WITHDRAWN' THEN tb.transactionid END) W,
                     COUNT(DISTINCT CASE WHEN tb.vl LIKE '%ESIGNED%' THEN tb.transactionid END) ES,
                     case when da.AppStatus is not null then da.AppStatus else 'NO APPLICATION' end AppStatus,
                     GROUP_CONCAT(distinct dr.RuleID) RuleIDs
                     FROM (SELECT tt.transactionid,
                     tlm.leadid,
                     tlm.applicationid,
                     tt.channel,
                     ln.Zipcode,
                     GROUP_CONCAT(value) vl,
                     CONVERT_TZ(MIN(tt.created_dtm),'UTC','US/Pacific') min_created_dtm,
                     CONVERT_TZ(MAX(tt.created_dtm),'UTC','US/Pacific') max_created_dtm,
                     tlm.lead_identifier lead_id
                     FROM personify.transaction_tracker tt
                     INNER JOIN (SELECT t.transactionid FROM personify.transaction t WHERE date(created_dtm)=date(convert_tz(NOW(),'UTC','US/PACIFIC'))) as trans using (transactionid)
                     LEFT JOIN personify.transaction_lead_map tlm USING(transactionid)
                     LEFT JOIN personify.landing ln using(transactionid)
                     GROUP BY 1) tb
                     LEFT JOIN decision.lead l ON tb.lead_id=l.id
                     LEFT JOIN decision.application da USING(lead_ID)
                     LEFT JOIN decision.zipcodes d ON tb.Zipcode = d.zipcode
                     LEFT JOIN decision.contact c USING(lead_ID)
                     LEFT JOIN decision.decisionrules dr ON l.id=dr.lead_id
                     WHERE date(min_created_dtm)=date(convert_tz(NOW(),'UTC','US/PACIFIC')) GROUP BY tb.transactionid with rollup;")
abc = fetch(result, n = 1000)
#abc$Time<-as.POSIXct(abc$Time, format="%yyyy-%mm-%dd")
abc$Time<-ymd(abc$Time)
#abc$Time<-as.character(abc$Time)
#print(abc)


#leaflet disbursal count

ghjip=dbSendQuery(mysqlconnection,"select c.state,sum(loan__Principal_Paid__c) as 'Amount Disbursed',sum(loan__Principal_Remaining__c) as 'Amount to get'  from cl_import.loan__Loan_Account__c llac
                  left join decision.loan l on  llac.name=l.sf_loan_number
                  left join decision.contact c on l.Contact_ContactID=c.contactid group by 1 having c.state is not null;")
fg12=fetch(ghjip,n=1000)




#hour wise
ytadu1=dbSendQuery(mysqlconnection,"select l.channel,count(distinct case when l.leadid is not null then l.leadid end) looks ,
                   count(distinct case when em.leadid is not null then em.leadid end) email_sent ,
                   count(distinct case when em.leadid is not null and em.is_email_verified = 1 then em.leadid end)email_verified,
                   count(distinct case when app.ApplicationID is not null then app.ApplicationID end) applications,
                   count(distinct case when app.ApplicationID is not null and (app.AppStatus is null or app.AppStatus = 'DROPPED') then app.ApplicationID end) drp,
                   count(distinct case when app.ApplicationID is not null and (app.AppStatus like '%REJECT') then app.ApplicationID end) decl,
                   count(distinct case when app.ApplicationID is not null and (app.AppStatus = 'PEND') and
                   ma.leadid is null then app.ApplicationID end)Pended,
                   count(distinct case when app.ApplicationID is not null and app.Appstatus = 'AUTO_APPROVE'
                   then app.ApplicationID end) apprvd,
                   count(distinct case when app.ApplicationID is not null and app.AppStatus='MANUAL_APPROVE' then app.Applicationid end)manual,
                   count(distinct case when app.ApplicationID is not null and app.isapproved = 1 and
                   ln.loanid is not null and e.LoanID is not null then e.LoanID end)Esigned, count(distinct case when
                   app.applicationid is not null and cl.loan__Loan_Status__c like '%Active%' then cl.Id end)Funded
                   from lead l left join lead_email_verified em on l.leadid = em.leadid left join application app
                   on l.id = app.lead_id left join  ManualApproval ma on app.LeadID = ma.LeadID
                   left join (select distinct leadid,ApproveStatus from stage11 where ApproveStatus = 'PEND')
                   s11 on s11.LeadID=app.LeadID left join   loan ln on app.ApplicationID=ln.Application_ApplicationID left join
                   Esign e on ln.LoanID=e.LoanID left join   (select * from cl_import.loan__Loan_Account__c where
                   CreatedDate >= date_sub(date(CONVERT_TZ(current_timestamp(),'UTC','US/Pacific')),interval 1 day)) cl on ln.sf_loan_id=cl.id where
                   l.Created_at >= date_sub(date(CONVERT_TZ(current_timestamp(),'UTC','US/Pacific')),interval 0 day) group by 1; ")
yth11=fetch(ytadu1,n=100)
#print (yth11)
yth22<-melt(yth11)



#Loans Booked
xwsq21= dbSendQuery(mysqlconnection,"select monthname(createdate) as Date,count(case when loan_originator='ADF' then loanid end) 'ADF',
                    count(case when loan_originator='FEB' then loanid end)'FEB'
                    from decision.loan 
                    where CreateDate>date_sub(convert_tz(now(),'UTC','US/PACIFIC'), interval 5 month) group by month(createdate);")
fg212= fetch(xwsq21, n = 1000)
fg2123<-melt(fg212)
#print(fg2123)


#disbursal transaction today
tab212= dbSendQuery(mysqlconnection,"select 
                    lldt.name,(lldt.loan__Disbursal_Date__c)'Disbursal Date' ,
                    llac.name,e.CreateDate
                    from decision.Esign e left join decision.loan l on l.Application_ApplicationID=e.ApplicationID
                    left join cl_import.loan__Loan_Account__c llac on l.sf_loan_number=llac.name
                    left join cl_import.loan__Loan_Disbursal_Transaction__c lldt on lldt.loan__Loan_Account__c=llac.id
                    where e.createdate between date_format(date_sub(convert_tz(current_timestamp(),'UTC','US/Pacific'),interval 2 day),'%Y-%m-%d 15:00:00') and date_format(date_sub(convert_tz(current_timestamp(),'UTC','US/Pacific'),interval 1 day),'%Y-%m-%d 15:00:00');")

tab21= fetch(tab212, n = 1000)


tab2122= dbSendQuery(mysqlconnection,"select count(distinct name) 'Count',loan__Loan_Status__c from cl_import.loan__Loan_Account__c group by 2;")

tab2131= fetch(tab2122, n = 1000)




##disbursal transaction tomorrow
tab213= dbSendQuery(mysqlconnection,"select 
                    lldt.name,(lldt.loan__Disbursal_Date__c)'Disbursal Date' ,
                    llac.name,e.CreateDate
                    from decision.Esign e left join decision.loan l on l.Application_ApplicationID=e.ApplicationID
                    left join cl_import.loan__Loan_Account__c llac on l.sf_loan_number=llac.name
                    left join cl_import.loan__Loan_Disbursal_Transaction__c lldt on lldt.loan__Loan_Account__c=llac.id
                    where e.createdate between date_format(date_sub(convert_tz(current_timestamp(),'UTC','US/Pacific'),interval 1 day),'%Y-%m-%d 15:00:00') and date_format((convert_tz(current_timestamp(),'UTC','US/Pacific')),'%Y-%m-%d 15:00:00')
                    group by 1;")
tab22= fetch(tab213, n = 1000)










#channel wise
z= dbSendQuery(mysqlconnection,"
               select distinct l.channel,count(*) as count from lead l left join application a on a.lead_id=l.id
               where date(a.createdatetime)=curdate() group by 1;
               ")

z1=fetch(z,n=100)
#print (z1)


xwsq= dbSendQuery(mysqlconnection,"select  code,s_name, count(applicationid) application, count(distinct case when applicationid is not null and appstatus like '%approve%' then ApplicationID end)approval
                  from reports.state_map left join contact on code = state left join application using(lead_id)  where s_name not in ('Hawaii','Alaska')  group by 1;")
fg= fetch(xwsq, n = 1000)
#print(fg)



xwsq1=dbSendQuery(mysqlconnection,"select *from decision.application order by id desc limit 100;")
fg1=fetch(xwsq1,n=1000)
#print (fg1)


#Leaflet Map
url <- "http://leafletjs.com/examples/choropleth/us-states.js"
doc <- readLines(url)
doc2 <- gsub("var statesData = ", "", doc)
write(doc2, file = "tempgeo.json")
states <- geojson_read("tempgeo.json", what = "sp")
#yu1<-merge(states,fg,by.x=c("name"),by.y=c("s_name"))
yu45<-merge(fg12,fg,by.x=c("state"),by.y=c("code"))
yu75<-merge(states,yu45,by.x=c("name"),by.y=c("s_name"))
options(scipen = 999)
states2<-spCbind(states,yu75$`Amount Disbursed`)






x1<-y$applications
x2<-y$autodecl
x3<-y$autoapprvd
x4<-y$Esigned
x5<-y$Funded
x6<-y$Curr_Pend
x11<-x1
x12<-x2
x13<-x3
x14<-x4
x15<-x5
x16<-x6
#x1<-y[which(y$appstatus== "applications"), ]
#x2<-y[which(y$appstatus== "autodecl"), ]
#x3<-y[which(y$appstatus== "autoapprvd"), ]
#x4<-y[which(y$appstatus== "Esigned"), ]
#x5<-y[which(y$appstatus== "Funded"), ]
#x6<-y[which(y$appstatus== "Curr_Pend"), ]
#x11<-x1[1,1]
#x12<-x2[1,1]
#x13<-x3[1,1]
#x14<-x4[1,1]
#x15<-x5[1,1]
#x16<-x6[1,1]
ze2<-ze1[1,1]
ze3<-ze4[1,1]

#leaflet tab
leaflettab1=dbSendQuery(mysqlconnection,"select sum(loan__Disbursed_Amt__c) as 'Amount Disbursed' from cl_import.loan__Loan_Disbursal_Transaction__c where date(loan__Disbursal_Date__c)=date_sub(curdate(),interval 1 day);")
lft1=fetch(leaflettab1,n=1000)

leaflettab2=dbSendQuery(mysqlconnection," select sum(loan__Transaction_Amount__c) as 'Amount to be pulled for the day' from cl_import.loan__Loan_Payment_Transaction__c  
                        where date(createddate)=date_sub(curdate(),interval 1 day) and IsDeleted=0 and loan__Payment_Mode__c in ('a1Dj0000002XbjREAS','a1Dj00000023VuGEAU','a1Dj00000023VuBEAU')
                        and loan__ACH_Filename__c is not null;")
lft2=fetch(leaflettab2,n=1000)

leaflettab3=dbSendQuery(mysqlconnection," select count(*) from decision.triggered_email where triggered_send_key like '%NOIA%' and date(created_date)=date_sub(curdate(),interval 1 day);")
lft3=fetch(leaflettab3,n=1000)

leaflettab4=dbSendQuery(mysqlconnection,"select count(*) from decision.triggered_email where triggered_send_key like '%ESIGN_REMINDER_LETTER%' and date(created_date)=date_sub(curdate(),interval 1 day);")
lft4=fetch(leaflettab4,n=1000)



ui <- dashboardPage(
  dashboardHeader(title = "Monitoring Dashboard"),
  dashboardSidebar( sidebarMenu(id = "sbm",
                                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                                menuItem("Disbursals and Transcations", tabName = "explorer", icon = icon("dollar")),
                                menuItem("Data Table",tabName="dta",icon=icon("dashboard")),
                                menuItem("Leaflet Map",tabName="xyz",icon=icon("object-group"))
  )# end of sidebarMenu
  ),#end of dashboardSidebar
  dashboardBody(
    tabItems(
      tabItem(tabName ="dashboard",
              fluidPage(title="Dashboard",theme = shinytheme("flatly"),  
                        fluidRow(
                          # A static valueBox
                          valueBox(x11, "Applications",icon = icon("smile-o"),width =3),
                          
                          # Dynamic valueBoxes
                          valueBox(x12,"Auto Decline",icon = icon("thumbs-down"),width =3),
                          
                          valueBox(x13,"Auto Approve",icon = icon("thumbs-up"),width =3),
                          valueBox(x14,"Esigned",icon=icon("smile-o"),width=3)
                        ),
                        fluidRow(
                          column(width=6,box(width =12,title="channel wise lead flow for the current day",status="primary",
                                             solidHeader = FALSE,
                                             collapsible = TRUE,showOutput("channelwise", "nvd3")
                          )
                          
                          ),
                          column(width=6,box(width =12,title="Loans Booked",status="primary",
                                             solidHeader = FALSE,
                                             collapsible = TRUE,showOutput("loans", "nvd3")
                          )
                          
                          )
                          
                        ),fluidRow(
                          column(width = 6,
                                 box(
                                   title = "Lead Flow With Status from current day to five previous days",
                                   status = "primary",
                                   width = 12,
                                   solidHeader = FALSE,
                                   collapsible = TRUE,
                                   showOutput("top10StatesTS", "nvd3")
                                 )
                          ),
                          column(width = 6,
                                 box(
                                   title = "Hour Wise Flow For The Day",
                                   status = "primary",
                                   width = 12,
                                   solidHeader = FALSE,
                                   collapsible = TRUE,
                                   showOutput("hoursf", "nvd3")
                                 )
                          )),
                        fluidRow(valueBox(x15,"Funded",icon=icon("smile-o"),width=3),
                                 valueBox(x16,"Current Pend",icon=icon("thumbs-o-down"),width=3),
                                 valueBox(ze2,"No of Loans Boooked",icon=icon("dollar"),width=3),
                                 valueBox(ze3,"No of Remediation Loans",icon=icon("dollar"),width=3)))
      ), # End of tabItem
      tabItem(tabName = "explorer",
              fluidPage(
                title = "Market Explorer",
                fluidRow( box(
                  title = "Value Growth by Value Scatterplot",
                  status = "primary",
                  width = "100%",height="100%",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  plotlyOutput("valueByGrowth")
                )
                
                ),
                fluidRow(
                  column(width=12,box( DT::dataTableOutput("table2"),width =12,title="Disbursal Details For Today",status="primary",
                                       solidHeader = FALSE,
                                       collapsible = TRUE))
                ),
                fluidRow(column(width=12,box(DT::dataTableOutput("table3"),width=12,solidHeader=FALSE,collapsible=TRUE,title="Disbursal Details For Tomorrow",status="primary")))
                
                
                
                
              )),
      
      tabItem(tabName = "dta",fluidPage(
        title="Basic DataTable",fluidRow(box(title="Flow of the customers today",status="primary",width=12,height=250,
                                             solidHeader = FALSE,
                                             collapsible = TRUE,
                                             
                                             # Create a new Row in the UI for selectInputs
                                             fluidRow(
                                               column(4,
                                                      selectInput("status",
                                                                  "Status:",
                                                                  c("All",
                                                                    unique(as.character(abc$AppStatus))))
                                               ),
                                               column(4,
                                                      selectInput("channel",
                                                                  "Channel:",
                                                                  c("All",
                                                                    unique(as.character(abc$Channel))))
                                               ),
                                               column(4,
                                                      selectInput("state",
                                                                  "State:",
                                                                  c("All",
                                                                    unique(as.character(abc$State))))
                                               )
                                             ),
                                             # Create a new row for the table.
                                             fluidRow(
                                               DT::dataTableOutput("table"),width=12,height=200,solidHeader = FALSE,
                                               collapsible = TRUE
                                             ),
                                             fluidRow(
                                               column(width=12,box( DT::dataTableOutput("summary"),width =12,title="Over All flow for the current day",status="primary",
                                                                    solidHeader = FALSE,
                                                                    collapsible = TRUE))
                                             ),
                                             fluidRow(
                                               column(width=12,box(plotlyOutput("plot") ,width ="auto",height=620,title="Over All flow for the current day",status="primary",
                                                                   solidHeader = FALSE,
                                                                   collapsible = TRUE))
                                             )
                                             
        )))),
      tabItem(tabName = "xyz",
              fluidPage(
                title = "abc",
                fluidRow( box(
                  title = "Leaflet Map",
                  status = "primary",
                  width = "100%",
                  height = "100%",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  leafletOutput("map")
                )
                
                ),
                fluidRow(valueBox(lft1,"Amount Disbursed For the day",icon=icon("dollar"),width=3),
                         valueBox(lft2,"Amount Pulled for the day",icon=icon("dollar"),width=3),
                         valueBox(lft3,"No of NOIA Mails Sent",icon=icon(" fa-envelope-o"),width=3),
                         valueBox(lft4,"No of NOD Mails Sent",icon=icon(" fa-envelope"),width=3)),
                fluidRow(
                  column(width=12,box( DT::dataTableOutput("table31"),width =12,title="Overall count",status="primary",
                                       solidHeader = FALSE,
                                       collapsible = TRUE))
                )
                
                
                
                
              ))
      
      
      
      
      
      
      
      
      
      
      
      
    )
  )
)
server <- function(input, output,session) {
  
  
  
  
  
  
  
  
  output$channelwise<-renderChart({
    #z1<-subset(z1,select=c(channel,count))
    p1<-nPlot(count~channel,data=z1,type="discreteBarChart",dom="channelwise")
    #p1$params$width <- 1000
    #p1$params$height <- 500
    p1$xAxis(axisLabel="Channel",staggerLabels = TRUE)
    p1$yAxis(axisLabel="count")
    return (p1)
  })
  
  output$loans<-renderChart({
    #z1<-subset(z1,select=c(channel,count))
    p21<-nPlot(value~Date,data=fg2123,group="variable",type="multiBarChart",dom="loans",height = 400, width = 680)
    #p21$params$width <- 1000
    #p21$params$height <- 500
    p21$xAxis(axisLabel="Channel",staggerLabels = TRUE)
    p21$yAxis(axisLabel="count")
    return (p21)
  })  
  output$top10StatesTS <- renderChart({
    
    # Plot Forecast
    p <- nPlot(value~Date,group ="status",type = "lineChart",data =yt17,dom = "top10StatesTS", height = 400, width = 680)
    p$xAxis(
      tickFormat =
        "#!
      function(d){
      f =  d3.time.format.utc('%Y-%m-%d');
      return f(new Date( d*24*60*60*1000 ));
      }
      !#"
    )
    p$yAxis(tickFormat = "#! function(d) {return d3.format(',.0f')(d)} !#")
    return(p)
})
  output$hoursf <- renderChart({
    
    # Plot Forecast
    p22 <- nPlot(value~channel,group ="variable",type = "multiBarChart",data =yth22,dom = "hoursf", height = 400, width = 680)
    return(p22)
  })
  
  output$valueByGrowth <- renderPlotly({
    
    fg$hover <- with(fg, paste(s_name, '<br>', "Approval", approval, "Application",application))
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    p <- plot_geo(fg, locationmode = 'USA-states') %>%
      add_trace(z=~approval,text =~hover, locations =~code,
                color = ~approval, colors = 'Purples'
      ) %>%
      colorbar(title = "Approval Rate") %>%
      layout(
        title = 'Approval count vs Application count',
        geo = g
      )
  })
  
  
  output$table2 <- renderDataTable({
    datatable(tab21, extensions = 'Scroller', options = list(
      deferRender = TRUE,
      scrollY = 200,
      scroller = TRUE
    ))
    
  })
  
  
  output$table31 <- renderDataTable({
    datatable(tab2131, extensions = 'Scroller', options = list(
      deferRender = TRUE,
      scrollY = 200,
      scroller = TRUE
    ))
    
  })
  
  
  output$table3<-renderDataTable({
    
    datatable(tab22, extensions = 'Scroller', options = list(
      deferRender = TRUE,
      scrollY = 200,
      scroller = TRUE
    ))
    
  })
  
  
  
  output$summary<-renderDataTable({
    
    x<-tail(abc,n=1L)
    y<-x[5:28]
    rownames(y)<-"TOTALFLOW"
    y
    
    
  })
  
  
  output$plot<-renderPlotly({
    x<-tail(abc,n=1L)
    y<-x[5:28]
    z<-print(y,row.names=FALSE)
    zy <- melt(z[, 1:ncol(z)])
    qy<-ggplot(zy, aes(x = variable, y = value,fill=variable)) + geom_bar(stat = "identity") + coord_flip()
    to<-ggplotly(qy)
    print(qy)
    
    
  })
  
  
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- abc
    
    if (input$status != "All") {
      data <- data[data$AppStatus == input$status,]
    }
    if (input$channel != "All") {
      data <- data[data$Channel == input$channel,]
    }
    if (input$state != "All") {
      data <- data[data$State == input$state,]
    }
    #if (input$date!="All"){
    #  data<-data[data$Time==input$date,]
    #}
    
    #if (input$dateRange!="All"){
    #  data<-data[data$Time==input$dateRange,]
    #}
    
    data
    
  }, 
  options = list(scrollX = TRUE,autowidth=TRUE)))
  output$map <- renderLeaflet({
    bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
    pal <- colorBin("YlOrRd", domain = states2$yu75..Amount.Disbursed., bins = bins)
    options(scipen = 999)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Amount Disbursed",
      states2$name, states2$yu75..Amount.Disbursed.
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states2,height="100%") %>%
      setView(-96, 37.8, 4) %>% 
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
      addPolygons(
        fillColor = ~pal(density),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~yu75..Amount.Disbursed., opacity = 0.7, title = NULL,
                position = "bottomright")
    
    
    
  })
  #format(output$map,scientific=FALSE)
  session$allowReconnect(TRUE)
  
  dbDisconnectAll <- function(){
    ile <- length(dbListConnections(MySQL())  )
    lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
    cat(sprintf("%s connection(s) closed.\n", ile))
  }
  dbDisconnectAll()
  gc()
  #session$onSessionEnded(stopApp)
  }





runApp(shinyApp(ui, server),host="172.31.12.242", port = 8095)



