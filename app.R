library(shiny)
library(shinydashboard)
library("ggplot2")
library(reshape2)
library(rsconnect)
library(plotly)
library(readxl)

ids <- read_excel("ids.xlsx")


css <- "
#large .selectize-input { line-height: 40px; }
#large .selectize-dropdown { line-height: 30px; }"

css2 <- "
#theorems .selectize-input {
    min-width: 1000px !important;
}
#theorems .selectize-input.not-full > input {
    width: 100% !important;
}
#theorems .selectize-dropdown-content {
                                       max-height: 700px; ## CHANGE THIS
                                       }
"


ui <- dashboardPage(
    dashboardHeader(title = "probability laws"),
    dashboardSidebar(
        sidebarMenu(id='menus',
                    menuItem(text = "Plotting some densities" , icon = icon("pencil", lib='glyphicon'),tabName = "density"),
                    menuItem(text = "probability calculator", icon = icon("cog", lib = 'glyphicon'),tabName = "calc"),
                    menuItem(text = "Theorems and definitions", tabName = "theos", icon=icon("education",lib='glyphicon'))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("density",
                    fluidRow(column(width = 11,offset = 3,box(uiOutput('dense'), title=strong('Density function :'), status='primary',collapsible = T,collapsed = T,solidHeader = T))),
                    fluidRow(
                        tabsetPanel(id = 'tabs',
                                    tabPanel(title='Uniforme',value='unif',fluidRow(
                                        column(8, plotlyOutput('graphe')),
                                        column(3,wellPanel(
                                            sliderInput(inputId = "inf",label = "borne inf",min = -10,max = 10,value = 0,step = 0.2),br(),
                                            sliderInput(inputId = "sup",label = "borne sup",min = -10,max = 10,value = 1,step = 0.2),br(),
                                            actionButton(inputId = 'resetunif',label = "reset"))
                                        ))),
                                    tabPanel(title='Normale',value='norm',fluidRow(
                                        column(8, plotlyOutput('graphenorme')),
                                        column(3,wellPanel(
                                            sliderInput(inputId = "mu",label = "mean",min = -10,max = 10,value = 0,step = 0.2),br(),
                                            sliderInput(inputId = "var",label = "variance",min = 0,max = 10,value = 1,step = 0.2),br(),
                                            actionButton(inputId = 'resetnorm',label = "reset"))
                                        ))),
                                    tabPanel(title = "Exponentielle", value='exp', fluidRow(
                                        column(8, plotlyOutput("grapheexp")),
                                        column(3, wellPanel(
                                            sliderInput(inputId = "lamb",label = "lambda", min = 0.1, max = 3, step = 0.1, value = 1),br(),
                                            actionButton(inputId = "resetexp", label='reset'))
                                        ))),
                                    tabPanel('title'='Gamma',value='gamma',fluidRow(
                                            column(8, plotlyOutput("plotgamma")),
                                            column(3,wellPanel(
                                                sliderInput(inputId = "kgamma",label = "k",min = 0.1,max = 10,value = 1,step = 0.1),br(),
                                                sliderInput(inputId = "thetagamma",label = "theta",min = 0.1,max = 10,value = 1,step = 0.1),br(),
                                                actionButton(inputId = "resetgamma",label = "reset"))
                                        ))),
                                    tabPanel('title'='Beta 1',value='beta',fluidRow(
                                        column(8, plotlyOutput("plotbeta")),
                                        column(3,wellPanel(
                                            sliderInput(inputId = "alphabeta",label = "alpha",min = 0.1,max = 10,value = 0.5,step = 0.1),br(),
                                            sliderInput(inputId = "betabeta",label = "beta",min = 0.1,max = 10,value = .5,step = 0.1),br(),
                                            actionButton(inputId = "resetbeta",label = "reset"))
                                        ))),
                                    tabPanel('title'='Cauchy',value='cauchy', fluidRow(
                                        column(8, plotlyOutput("plotcauchy")),
                                        column(3,wellPanel(
                                            sliderInput(inputId = "xcauchy",label = "position",min = -5,max = 5,value = 0,step = 0.1),br(),
                                            sliderInput(inputId = "acauchy",label = "a",min = 0.1,max = 10,value = 1,step = 0.1),br(),
                                            actionButton(inputId = "resetcauchy",label = "reset"))
                                        ))),
                                    tabPanel('title'='Arcsinus',value='asin',  fluidRow(
                                        column(8, plotlyOutput("plotasin")),
                                        column(3,wellPanel(
                                            sliderInput(inputId = "aasin",label = "a",min = -10,max = 10,value = 0,step = 0.1),br(),
                                            sliderInput(inputId = "basin",label = "b",min = -10 ,max = 10,value = 1 ,step = 0.1),br(),
                                            actionButton(inputId = "resetasin",label = "reset"))
                                        ))),
                                    tabPanel('title'='Weibull',value='weib', fluidRow(
                                        column(8, plotlyOutput("plotweib")),
                                        column(3,wellPanel(
                                            sliderInput(inputId = "lambweib",label = "scale",min = 0.1,max = 10,value = .5,step = 0.1),br(),
                                            sliderInput(inputId = "kweib",label = "location",min = 0.1 ,max = 10,value = 2 ,step = 0.1),br(),
                                            actionButton(inputId = "resetweib",label = "reset"))
                                        ))),
                                    tabPanel('title'='Gumbel',value='gum', fluidRow(
                                        column(8, plotlyOutput("plotgum")),
                                        column(3,wellPanel(
                                            sliderInput(inputId = "mugum",label = "location",min = -10,max = 10,value = 0,step = 0.1),br(),
                                            sliderInput(inputId = "betagum",label = "scale",min = 0.1 ,max = 10,value = 1 ,step = 0.1),br(),
                                            actionButton(inputId = "resetgum",label = "reset"))
                                        ))),
                                    tabPanel('title'='Log-normale',value='logn', fluidRow(
                                        column(8, plotlyOutput("plotlogn")),
                                        column(3,wellPanel(
                                            sliderInput(inputId = "mulogn",label = "mu",min = -10,max = 10,value = 0,step = 0.1),br(),
                                            sliderInput(inputId = "varlogn",label = "variance",min = 0.01 ,max = 2,value = 1 ,step = 0.01),br(),
                                            actionButton(inputId = "resetlogn",label = "reset"))
                                        ))),
                                    tabPanel('title'='Khi 2',value='khi', fluidRow(
                                        column(8, plotlyOutput("plotkhi")),
                                        column(3,wellPanel(
                                            sliderInput(inputId = "kkhi",label = "degrees of freedom",min = 1 ,max = 20,value = 1 ,step = 1),br(),
                                            actionButton(inputId = "resetkhi",label = "reset"))
                                        ))),
                                    tabPanel('title'='Student',value='stud', fluidRow(
                                        column(8, plotlyOutput("plotstud")),
                                        column(3,wellPanel(
                                            sliderInput(inputId = "kstud",label = "degrees of freedom",min = 1 ,max = 20,value = 1 ,step = 1),br(),
                                            actionButton(inputId = "resetstud",label = "reset"))
                                        ))),
                                    tabPanel('title'='Pareto',value='par', fluidRow(
                                        column(8, plotlyOutput("plotpar")),
                                        column(3,wellPanel(
                                            sliderInput(inputId = "xpar",label = "location",min = 0.1,max = 10,value = 1,step = 0.1),br(),
                                            sliderInput(inputId = "kpar",label = "scale",min = 0.01 ,max = 10,value = 1 ,step = 0.1),br(),
                                            actionButton(inputId = "resetpar",label = "reset"))
                                        ))),
                                    tabPanel('title'='Fisher',value='fish', fluidRow(
                                        column(8, plotlyOutput("plotfish")),
                                        column(3,wellPanel(
                                            sliderInput(inputId = "d1",label = "degree of freedom 1",min = 1,max = 100,value = 1,step = 1),br(),
                                            sliderInput(inputId = "d2",label = "degree of freedom 2",min = 1 ,max = 100,value = 1 ,step = 1),br(),
                                            actionButton(inputId = "resetfish",label = "reset"))
                                        ))),
                                    tabPanel('title'='Logistique',value='logi',fluidRow(
                                        column(8, plotlyOutput("plotlogi")),
                                        column(3,wellPanel(
                                            sliderInput(inputId = "mulogi",label = "mu",min = -10,max = 10,value = 0,step = .1),br(),
                                            sliderInput(inputId = "slogi",label = "s",min = .1 ,max = 10,value = 1 ,step = 1),br(),
                                            actionButton(inputId = "resetlogi",label = "reset"))
                                        )))
                                    
                    )
            ),
            fluidRow(br()),
            fluidRow(box(uiOutput('expected'),title = strong('Formula for expectancy'), status='info',color='light-blue',collapsible=T, collapsed=T, solidHeader=T),
                     box(uiOutput('variance'),title = strong('Formula for variance'), status='info',color='light-blue',collapsible=T, collapsed=T, solidHeader=T)),
            fluidRow(infoBoxOutput(outputId = "esp",width = 6),
                     infoBoxOutput(outputId = "var",width = 6))
            ),
            tabItem("calc",fluidRow(
                    box(title = strong('Choose your law'), status = 'primary', solidHeader = T,
                    tags$style(type='text/css', css),
                    div(id = "large",
                        selectInput("law", "", c('Uniforme'='unif','Normale'='norm','Exponentielle'='exp','Gamma'='gamma','Beta I'='beta','Cauchy'='cauchy','Arcsin'='asin','Weibull'='weib','Gumbel'='gum','Log-normale'='lnorm','Khi 2'='khi','Student'='stud','Pareto'='par','Fisher'='fish','Logistique'='logi'))
                    )),
                    box(title=strong('Choose the side'),status = 'primary', solidHeader = T,
                        radioButtons('side',label = '',inline=T,choices = c('Unilateral left'='unileft', 'Unilateral right'='uniright','Bilateral' ='bilateral')),
                        uiOutput('bornes')
                        )
                        ),
                    fluidRow(br()),
                    fluidRow(column(10,box(title=strong('Parameters'),status='primary', solidHeader=T,uiOutput('param'))),column(10,wellPanel(textOutput('calcul')))),
                    fluidRow(br()),
                    fluidRow(column(width = 11,offset = 3,box(uiOutput('repartition'), title=strong('Repartition function :'), status='primary',collapsible = T,collapsed = T,solidHeader = T)))
            ),
            tabItem("theos",
                    fluidRow(column(12,tags$style(type='text/css',css2),div(id="theorems",uiOutput('foo')))),
                    #withMathJax(sprintf('If \\(X\\leq 3\\) is a Cauchy $$P(X\\leq %.03f)=%.03f$$',3, pcauchy(3))),
                    wellPanel(uiOutput('theoreme'))
            )   
        )
    )
    
)

server <- function(input, output,session) {

    x <- reactive({switch (input$tabs,
                           'unif' = seq(-10,10,0.1),
                           'norm' = seq(-10,10,0.1),
                           'exp' = seq(0,20,0.1),
                           'gamma' = seq(0,20,0.1),
                           'beta' = seq(0,1,0.01),
                           'cauchy' = seq(-5,5,.1),
                           'asin' = seq(min(0,input$aasin)+0.1,max(1,input$basin)-0.1,0.01),
                           'weib' = seq(0,5,0.05),
                           'gum' = seq(-5,20,0.1),
                           'logn' = seq(0,5,.01),
                           'khi' = seq(0,20,0.1),
                           'stud' = seq(-5,5,.1),
                           'par' = seq(0,input$xpar+5,.1),
                           'fish' = seq(0,5,.01),
                           'logi' = seq(-5,30,.1)
    )})
    
    lasin <- function(x,a,b){return(1/(pi*sqrt((b-x)*(x-a))))}
    gumb <- function(x,mu,beta){
        z <- exp(-(x-mu)/beta)
        return(exp(-z)*z/beta)
    }
    indicatrice <- function(x,a,b){return(ifelse(x >= a & x <= b, 1,0))}
    dpar <- function(x,k,l){return((k*l/(x**(k+1)))*indicatrice(x,l,Inf))}
    dlogi <- function(x,mu,s){ e <- exp(-(x-mu)/s)
        return(e/(s*(1+e)**2))}
    
    data <- reactive({switch(input$tabs,
                                   'unif' = dunif(x(),0,1),
                                   'norm' = dnorm(x(),0,1),
                                   'exp' = dexp(x(),1),
                                   'gamma' = dgamma(x(),1,1),
                                    'beta' = dbeta(x(),.5,.5),
                                    'cauchy' = dcauchy(x(),location = 0, scale = 1),
                                    'asin' = lasin(x(),0,1),
                                    'weib' = dweibull(x(),scale = 0.5, shape = 2),
                                    'gum' = gumb(x(),0,1),
                                    'logn' = dlnorm(x(),0,1),
                                    'khi' = dchisq(x(),df = 1),
                                    'stud' = dt(x(),1),
                                    'par' = dpar(x(),1,1),
                                    'fish' = df(x(),1,1),
                                    'logi' = dlogi(x(),0,1)
                                    )})
    
    data2 <- reactive({switch(input$tabs,
                              'unif' = dunif(x(),min(input$inf, input$sup),max(input$inf,input$sup)),
                              'norm' = dnorm(x(), input$mu, sqrt(input$var)) ,
                              'exp' = dexp(x(),input$lamb),
                              'gamma' = dgamma(x(),shape = input$kgamma, scale = input$thetagamma),
                              'beta' = dbeta(x(),input$alphabeta, input$betabeta),
                              'cauchy' = dcauchy(x(),location = input$xcauchy, scale = input$acauchy),
                              'asin' = dcauchy(x(),0,1),
                              'weib' = dweibull(x(),scale = input$lambweib, shape = input$kweib),
                              'gum' = gumb(x(),input$mugum, input$betagum),
                              'logn' = dlnorm(x(),input$mulogn, sqrt(input$varlogn)),
                              'khi' = dchisq(x(),input$kkhi),
                              'stud' = dt(x(), input$kstud),
                              'par' = dpar(x(),input$kpar,input$xpar),
                              'fish' = df(x(),input$d1,input$d2),
                              'logi' = dlogi(x(),input$mulogi,input$slogi)
     )})
    
    minimumx <- reactive({switch(input$tabs,
                                    'unif' = -10,
                                    'norm' = -10,
                                    'exp' = 0,
                                    'gamma' = 0,
                                    'beta' = 0,
                                    'cauchy' = -5,
                                    'asin' = min(0,input$aasin)+0.1,
                                    'weib' = 0,
                                    'gum' = -5,
                                    'logn' = 0,
                                    'khi' = 0,
                                    'stud' = -5,
                                    'par' = 0,
                                    'fish' = 0,
                                    'logi' = -5
                                   )})
    
    maximumx <- reactive({switch (input$tabs,
                                    'unif' = 10,
                                    'norm' = 10,
                                    'exp' = 20,
                                    'gamma' = 20,
                                    'beta' = 1,
                                    'cauchy' = 5,
                                    'asin' = max(1,input$basin)-0.1,
                                    'weib' = 5,
                                    'gum' = 20,
                                    'logn' = 3,
                                    'khi' = 20,
                                    'stud' = 5,
                                    'par' = input$xpar+5,
                                    'fish' = 5,
                                    'logi' = 30
                                    )})
    
    minimumy <- reactive({switch(input$tabs,
                                    'unif' = 0,
                                    'norm' = 0,
                                    'exp' = 0,
                                    'gamma' = 0,
                                    'beta' = 0,
                                    'cauchy' = 0,
                                    'asin' = 0,
                                    'weib' = 0,
                                    'gum' = 0,
                                    'logn' = 0,
                                    'khi' = 0,
                                    'stud' = 0,
                                    'par' = 0,
                                    'fish' = 0,
                                    'logi' = 0
    )})
    
    maximumy <- reactive({switch (input$tabs,
                                        'unif' = max(1/(max(input$sup,input$inf)-min(input$sup,input$inf))+0.1, 1),
                                        'norm' = .45,
                                        'exp' = input$lamb+1,
                                        'gamma' = 0.5,
                                        'beta' = 2.6,
                                        'cauchy' = .7,
                                        'asin' = 3,
                                        'weib' = 2,
                                        'gum' = 0.5,
                                        'logn' = 2.5,
                                        'khi' = .5,
                                        'stud' = .5,
                                        'par' = 1.1,
                                        'fish' = 2.5,
                                        'logi' = .3
    )})
        
    espinput <- reactive({switch (input$tabs,
                                'unif' = (input$inf+input$sup)/2,
                                'norm' = input$mu,
                                'exp' = 1/input$lamb,
                                'gamma'= input$kgamma*input$thetagamma,
                                'beta' = (input$alphabeta)/(input$alphabeta+input$betabeta),
                                'cauchy' = 'Non defined',
                                'asin' = (input$aasin+input$basin)/2,
                                'weib' = input$lambweib*gamma(1+1/input$kweib),
                                'gum' = input$mugum + input$betagum*(-digamma(1)),
                                'logn' = exp(input$mulogn+input$varlogn/2),
                                'khi' = input$kkhi,
                                'stud' = {if(input$kstud == 1){'Non defined'} else{0}},
                                'par' = {if(input$kpar<=1){'Infinity'} else{input$kpar*input$xpar/(input$kpar-1)}},
                                'fish' = {if(input$d2<=2){'Non defined'} else{input$d2/(input$d2-2)}},
                                'logi' = input$mulogi
                                    )})
    
    varinput <- reactive({switch (input$tabs,
                                'unif' = (input$sup-input$inf)**2/12,
                                'norm' = input$var,
                                'exp' = 1/input$lamb**2,
                                'gamma' = (input$kgamma*input$thetagamma**2),
                                'beta' = input$alphabeta*input$betabeta/(((input$alphabeta+input$betabeta)**2)*(input$alphabeta+1+input$betabeta)),
                                'cauchy' = 'Non defined',
                                'asin' = (input$basin-input$aasin)**2/8,
                                'weib' = input$lambweib**2*gamma(1+2/input$kweib)-espinput()**2,
                                'gum' = pi**2/6*input$betagum**2,
                                'logn' = exp(input$varlogn-1)*exp(2*input$mulogn+input$varlogn),
                                'khi' = 2*input$kkhi,
                                'stud' = {if(input$kstud==1){'Non defined'}else if(input$kstud==2){'Infinity'} else {input$kstud/(input$kstud-2)}},
                                'par' = {if(input$kpar<=2){'Infinity'} else {input$xpar**2*input$kpar/((input$kpar-1)**2*(input$kpar-2))}},
                                'fish' = {ifelse(input$d2<=4,'Non defined',2*input$d2**2*(input$d1+input$d2-2)/(input$d1*(input$d2-2)**2*(input$d2-4)))},
                                'logi' = input$slogi**2*pi/3
                                    )})
    
    output$esp <- renderInfoBox({infoBox(title='The expected value', subtitle=' ', value=paste('mean =', as.character(espinput()),collapse = ' '), color = 'light-blue', icon = icon('angle-double-up'),fill = T)})
    output$var <- renderInfoBox({infoBox(title='The variance ',subtitle='     ',value={paste('variance =', as.character(varinput()),collapse=' ')},color='blue', icon=icon("thumbs-up", lib = "glyphicon"),fill=T)})
    
    observeEvent(input$resetunif, {updateSliderInput(session, 'inf',value=0); updateSliderInput(session, 'sup',value=1)})
    observeEvent(input$resetnorm, {updateSliderInput(session, 'mu',value=0); updateSliderInput(session, 'var',value=1)})
    observeEvent(input$resetexp, {updateSliderInput(session, 'lamb',value=1)})
    observeEvent(input$resetgamma, {updateSliderInput(session, 'kgamma',value=1);updateSliderInput(session, 'thetagamma',value=1)})
    observeEvent(input$resetbeta, {updateSliderInput(session, 'alphabeta',value=0.5);updateSliderInput(session, 'betabeta',value=0.5)})
    observeEvent(input$aasin, {updateSliderInput(session = session, 'basin', min=input$aasin+0.01)})
    observeEvent(input$resetasin, {updateSliderInput(session, 'aasin',value=0);updateSliderInput(session, 'basin',value=1)})
    observeEvent(input$resetweib, {updateSliderInput(session, 'lambweib', value=0.5);updateSliderInput(session, 'kweib', value=2)})
    observeEvent(input$resetgum, {updateSliderInput(session, 'mugum', value=0);updateSliderInput(session, 'betagum', value=1)})
    observeEvent(input$resetlogn, {updateSliderInput(session, 'mulogn', value=0);updateSliderInput(session, 'varlogn', value=1)})
    observeEvent(input$resetkhi, {updateSliderInput(session = session, 'kkhi', value=1)})
    observeEvent(input$resetstud, {updateSliderInput(session = session, 'kstud', value=1)})
    observeEvent(input$resetpar, {updateSliderInput(session, 'xpar', value=1);updateSliderInput(session, 'kpar', value=1)})
    observeEvent(input$resetfish, {updateSliderInput(session, 'd1', value=1);updateSliderInput(session, 'd2', value=1)})
    observeEvent(input$resetlogi, {updateSliderInput(session, 'mulogi', value=0);updateSliderInput(session, 'slogi', value=1)})
    
    
    
    output$graphe <- output$graphenorme <- output$grapheexp <- output$plotgamma <- output$plotbeta <- output$plotcauchy <- output$plotasin <- output$plotweib <- output$plotgum <- output$plotlogn <-
            output$plotkhi <- output$plotstud <- output$plotpar <- output$plotfish <- output$plotlogi <- renderPlotly({
                                df <- melt(data.frame(x(),data(),data2()), id='x..')
                                 p <- ggplot(data=df, aes(x=x.., y=value, colour=variable)) + geom_line() + xlim(minimumx(),maximumx()) + ylim(minimumy(),maximumy()) + theme(legend.position = 'none')
                                 p <- p + labs(x = 'x', y = 'f(x)') + scale_fill_discrete(name = "Values", labels = c("Reference", "New"))
                                print(ggplotly(p,tooltip = c('y')))
                                 })
    
    output$expected <- renderUI({p({switch(input$tabs,'unif' = withMathJax('$$\\mathbb{E}[X]=\\dfrac{a+b}{2}$$'),
                                                    'norm' = withMathJax('$$\\mathbb{E}[X]=\\mu$$'),
                                                    'exp' = withMathJax('$$\\mathbb{E}[X]=\\dfrac{1}{\\lambda}$$'),
                                                    'gamma' = withMathJax('$$\\mathbb{E}[X]=k\\theta$$'),
                                                    'beta' = withMathJax('$$\\mathbb{E}[X]=\\dfrac{\\alpha}{\\alpha+\\beta}$$'),
                                                    'asin' = withMathJax('$$\\mathbb{E}[X]=\\dfrac{a+b}{2}$$'),
                                                    'cauchy' = withMathJax('$$\\mathbb{E}[X]=\\varnothing$$'),
                                                    'weib' = withMathJax('$$\\mathbb{E}[X]=\\lambda\\Gamma\\left(1+\\dfrac{1}{k}\\right)$$'),
                                                    'gum' = withMathJax('$$\\mathbb{E}[X]=\\mu+\\beta\\gamma,\\textrm{ with }\\gamma=-\\Gamma\'(1)$$'),
                                                    'logn' = withMathJax('$$\\mathbb{E}[X]=e^{\\mu+\\frac{\\sigma^2}{2}}$$'),
                                                    'khi' = withMathJax('$$\\mathbb{E}[X]=k$$'),
                                                    'stud' = withMathJax('$$\\mathbb{E}[X]=0 \\textrm{ for } k>1$$'),
                                                    'par' = withMathJax('$$\\mathbb{E}[X]=\\dfrac{kx_m}{k-1},\\textrm{ for} k>1 $$'),
                                                    'fish' = withMathJax('$$\\mathbb{E}[X]=\\dfrac{d_2}{d_2-2},\\textrm{ for }k>2$$'),
                                                    'logi' = withMathJax('$$\\mathbb{E}[X]=\\mu$$')
                                        )})})
    
    output$dense <- renderUI({p({switch(input$tabs,'unif' = withMathJax('$$f(x)=\\dfrac{1}{b-a}\\textbf{1}\\{a\\leq x \\leq b\\}$$'),
                                        'norm' = withMathJax('$$f(x)=\\dfrac{1}{\\sigma\\sqrt{2\\pi}}e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}}$$'),
                                        'exp' = withMathJax('$$f(x)=\\lambda e^{-\\lambda x}\\textbf{1}_{\\mathbb{R}^+}$$'),
                                        'gamma' = withMathJax('$$f(x)=\\dfrac{x^{k-1}e^{-\\frac{x}{\\theta}}}{\\Gamma(k)e^k}\\textbf{1}_{\\mathbb{R}^+},\\textrm{ with } \\Gamma(z)=\\int_{0}^{+\\infty}t^{z-1}e^{-t}\\mathrm{d}t,\\quad \\Gamma(x+1)=x\\Gamma(x)$$'),
                                        'beta' = withMathJax('$$f(x)=\\dfrac{x^{\\alpha-1}(1-x)^{\\beta-1}}{\\textbf{B}(\\alpha,\\beta)} \\textbf{1}_{[0,1]},\\textrm{ with } \\textbf{B}(x,y)=\\dfrac{\\Gamma(x)\\Gamma(y)}{\\Gamma(x+y)}$$'),
                                        'asin' = withMathJax('$$f(x)=\\dfrac{1}{\\pi\\sqrt{(x-a)(b-x)}}\\textbf{1}_{[a,b]}$$'),
                                        'cauchy' = withMathJax('$$f(x)=\\dfrac{1}{\\pi a\\left[1+\\left(\\dfrac{x-x_0}{a}\\right)^2\\right]}$$'),
                                        'weib' = withMathJax('$$f(x)=\\dfrac{k}{\\lambda}\\left(\\dfrac{x}{\\lambda}\\right)^{k-1}e^{-\\left(\\frac{x}{\\lambda}\\right)^k}\\textbf{1}_{\\mathbb{R}^+}$$'),
                                        'gum' = withMathJax('$$f(x)=\\dfrac{e^{-z}z}{\\beta}\\textrm{ with }z=e^{-\\frac{x-\\mu}{\\beta}}$$'),
                                        'logn' = withMathJax('$$f(x)=\\dfrac{1}{x\\sigma\\sqrt{2\\pi}}\\mathrm{exp}\\left(-\\dfrac{(\\ln x-\\mu)^2}{2\\sigma^2}\\right)\\textbf{1}_{\\mathbb{R}^+_*}$$'),
                                        'khi' = withMathJax('$$f(x)=\\dfrac{\\left(\\frac{1}{2}\\right)^{\\frac{k}{2}}}{\\Gamma\\left(\\frac{k}{2}\\right)}x^{\\frac{k}{2}-1}e^{-\\frac{x}{2}}\\mathrm{1}_{\\mathbb{R}^+}$$'),
                                        'stud' = withMathJax('$$f(x)=\\dfrac{1}{\\sqrt{k\\pi}}\\dfrac{\\Gamma\\left(\\frac{k+1}{2}\\right)}{\\Gamma\\left(\\frac{k}{2}\\right)}\\left(1+\\dfrac{x^2}{k}\\right)^{-\\frac{k+1}{2}}$$'),
                                        'par' = withMathJax('$$f(x)=\\dfrac{kx_m^k}{x^{k+1}}\\textbf{1}_{[x_m,+\\infty[}$$'),
                                        'fish' = withMathJax('$$f(x)=\\dfrac{\\sqrt{\\dfrac{(d_1x)^{d_1}d_2^{d_2}}{(d_1x+d_2)^{d_1+d_2}}}}{x\\textbf{B}\\left(\\frac{d_1}{2},\\frac{d_2}{2}\\right)}\\textbf{1}_{\\mathbb{R}^+},\\textrm{ with }\\textbf{B}(x,y)=\\dfrac{\\Gamma(x)\\Gamma(y)}{\\Gamma(x+y)}$$'),
                                        'logi' = withMathJax('$$f(x)=\\dfrac{e^{-\\frac{(x-\\mu)}{s}}}{s\\left(1+e^{-\\frac{(x-\\mu)}{s}}\\right)^2}$$')
    )})})
    
    output$variance <- renderUI({p({switch(input$tabs,'unif' = withMathJax('$$\\mathbb{V}[X]=\\dfrac{(b-a)^2}{12}$$'),
                                           'norm' = withMathJax('$$\\mathbb{V}[X]=\\sigma^2$$'),
                                           'exp' = withMathJax('$$\\mathbb{V}[X]=\\dfrac{1}{\\lambda^2}$$'),
                                           'gamma' = withMathJax('$$\\mathbb{V}[X]=k\\theta^2$$'),
                                           'beta' = withMathJax('$$\\mathbb{V}[X]=\\dfrac{\\alpha\\beta}{(\\alpha+\\beta)^2(\\alpha+\\beta+1)}$$'),
                                           'cauchy' = withMathJax('$$\\mathbb{V}[X]=\\varnothing$$'),
                                           'asin' = withMathJax('$$\\mathbb{V}[X]=\\dfrac{(b-a)^2}{8}$$'),
                                           'weib' = withMathJax('$$\\mathbb{V}[X]=\\lambda^2\\Gamma\\left(1+\\dfrac{2}{k}\\right)-\\mu^2$$'),
                                           'gum' = withMathJax('$$\\mathbb{V}[X]=\\dfrac{\\pi^2}{6}\\beta^2$$'),
                                           'logn' = withMathJax('$$\\mathbb{V}[X]=\\left(e^{\\sigma^2}-1\\right)e^{2\\mu+\\sigma^2}$$'),
                                           'khi' = withMathJax('$$\\mathbb{V}[X]=2k$$'),
                                           'stud' = withMathJax('$$\\mathbb{V}[X]=\\dfrac{k}{k-2}\textrm{ with } k>2$$'),
                                           'par' = withMathJax('$$\\mathbb{V}[X]=\\dfrac{x_m^2k}{(k-1)^2(k-2)}\textrm{ with } k>2$$'),
                                           'fish' = withMathJax('$$\\mathbb{V}[X]=\\dfrac{2d_2^2(d_1+d_2-2)}{d_1(d_2-2)^2(d_2-4)}$$'),
                                           'logi' = withMathJax('$$\\mathbb{V}[X]=\\dfrac{\\pi^2}{3}s^2$$')
    )})})
    
    labelbornes <- reactive(paste('Just enter the unknown value',switch(input$side, 'unileft'=' : -Inf ; x',
                                                                          'uniright' = ' : x ; +Inf',
                                                                          'bilateral' = 's : x ; y')
                                  ,sep=''))
    
    output$bornes <- renderUI({
        textInput('borneschosed', label=strong('Where to ?'), placeholder = labelbornes())})
    
    labelaparam <- reactive(paste('(Enter them this way) ',switch(input$law,'unif' = 'a ; b ;',
                            'norm' = 'mu ; sigma^2',
                            'exp' = 'lambda' ,
                            'gamma' = 'k ; theta',
                            'beta' = 'alpha ; beta',
                            'cauchy' = 'x_0 ; a',
                            'asin' = 'a ; b with a < b',
                            'weib' = 'lambda ; k',
                            'gum' = 'mu ; beta',
                            'lnorm' = 'mu ; sigma^2',
                            'khi' = 'k',
                            'stud' = 'k',
                            'par' = 'x_m ; k',
                            'fish' = 'd_1 ; d_2',
                            'logi' = 'mu ; s'), sep = ' '))
    
    
   output$param <- renderUI({textInput('params',label=strong('The parameters :'),placeholder = labelaparam())})
  
  repsin <- function(x,a,b){2/pi*asin(sqrt((x-a)/(b-a)))}
  repgumb <- function(x,mu,beta){exp(-exp(-(x-mu)/beta))}
  reppar <- function(x,x_m,k){1-(x_m/x)**k}
  replogi <- function(x,mu,s){1/2+1/2*tanh((x-mu)/2*s)}
  
  resultcalc <- reactive(switch (input$side,
      'unileft' = switch(input$law,'unif' = punif(q = as.numeric(input$borneschosed),min = as.numeric(strsplit(input$params,' ;')[[1]][1]), max= as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                         'norm' = pnorm(q=as.numeric(input$borneschosed),mean = as.numeric(strsplit(input$params, ' ;')[[1]][1]),sd = sqrt(as.numeric(strsplit(input$params, ' ;')[[1]][2]))),
                         'exp' = pexp(q=as.numeric(input$borneschosed),rate = as.numeric(input$params)),
                         'gamma' = pgamma(q=as.numeric(input$borneschosed),shape = as.numeric(strsplit(input$params, ' ;')[[1]][1]),scale = as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                         'beta' = pbeta(q=as.numeric(input$borneschosed), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                         'cauchy' = pcauchy(q=as.numeric(input$borneschosed), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                         'asin' = repsin(x=as.numeric(input$borneschosed), a=as.numeric(strsplit(input$params, ' ;')[[1]][1]), b=as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                         'weib' = pweibull(q=as.numeric(input$borneschosed), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                         'gum' = repgumb(x=as.numeric(input$borneschosed), mu=as.numeric(strsplit(input$params, ' ;')[[1]][1]), beta=as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                         'lnorm' = dlnorm(as.numeric(input$borneschosed), as.numeric(strsplit(input$params, ' ;')[[1]][1]), sqrt(as.numeric(strsplit(input$params, ' ;')[[1]][2]))),
                         'khi' = dchisq(as.numeric(input$borneschosed),df = as.numeric(input$params)),
                         'stud' = dt(as.numeric(input$borneschosed),df = as.numeric(input$params)),
                         'par' = reppar(x=as.numeric(input$borneschosed), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                         'fish' = df(as.numeric(input$borneschosed), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                         'logi' = replogi(as.numeric(input$borneschosed), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2]))),
    'uniright' = switch(input$law,'unif' = 1-punif(q = as.numeric(input$borneschosed),min = as.numeric(strsplit(input$params,' ;')[[1]][1]), max= as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                        'norm' = 1-pnorm(q=as.numeric(input$borneschosed),mean = as.numeric(strsplit(input$params, ' ;')[[1]][1]),sd = sqrt(as.numeric(strsplit(input$params, ' ;')[[1]][2]))),
                        'exp' = 1-pexp(q=as.numeric(input$borneschosed),rate = as.numeric(input$params)),
                        'gamma' = 1-pgamma(q=as.numeric(input$borneschosed),shape = as.numeric(strsplit(input$params, ' ;')[[1]][1]),scale = as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                        'beta' = 1-pbeta(q=as.numeric(input$borneschosed), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                        'cauchy' = 1-pcauchy(q=as.numeric(input$borneschosed), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                        'asin' = 1-repsin(x=as.numeric(input$borneschosed), a=as.numeric(strsplit(input$params, ' ;')[[1]][1]), b=as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                        'weib' = 1-pweibull(q=as.numeric(input$borneschosed), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                        'gum' = 1-repgumb(x=as.numeric(input$borneschosed), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                        'lnorm' = 1-dlnorm(as.numeric(input$borneschosed), as.numeric(strsplit(input$params, ' ;')[[1]][1]), sqrt(as.numeric(strsplit(input$params, ' ;')[[1]][2]))),
                        'khi' = 1-dchisq(as.numeric(input$borneschosed),df = as.numeric(input$params)),
                        'stud' = 1-dt(as.numeric(input$borneschosed),df = as.numeric(input$params)),
                        'par' = 1-reppar(x=as.numeric(input$borneschosed), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                        'fish' = 1-df(as.numeric(input$borneschosed), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                        'logi' = 1-replogi(as.numeric(input$borneschosed), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2]))),
    'bilateral' = switch(input$law,
                         'unif' = punif(q = as.numeric(strsplit(input$borneschosed, ' ;')[[1]][2]),min = as.numeric(strsplit(input$params,' ;')[[1]][1]), max= as.numeric(strsplit(input$params, ' ;')[[1]][2]))-punif(q = as.numeric(strsplit(input$borneschosed, ' ;')[[1]][1]),min = as.numeric(strsplit(input$params,' ;')[[1]][1]), max= as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                         'norm' = pnorm(q=as.numeric(strsplit(input$borneschosed, ' ;')[[1]][2]),mean = as.numeric(strsplit(input$params, ' ;')[[1]][1]),sd = sqrt(as.numeric(strsplit(input$params, ' ;')[[1]][2])))-pnorm(q=as.numeric(strsplit(input$borneschosed, ' ;')[[1]][1]),mean = as.numeric(strsplit(input$params, ' ;')[[1]][1]),sd = sqrt(as.numeric(strsplit(input$params, ' ;')[[1]][2]))),
                         'exp' = pexp(q=as.numeric(strsplit(input$borneschosed, ' ;')[[1]][2]),rate = as.numeric(input$params))-pexp(q=as.numeric(strsplit(input$borneschosed, ' ;')[[1]][1]),rate = as.numeric(input$params)),
                         'gamma' = pgamma(q=as.numeric(strsplit(input$borneschosed, ' ;')[[1]][2]),shape = as.numeric(strsplit(input$params, ' ;')[[1]][1]),scale = as.numeric(strsplit(input$params, ' ;')[[1]][2]))-pgamma(q=as.numeric(strsplit(input$borneschosed, ' ;')[[1]][1]),shape = as.numeric(strsplit(input$params, ' ;')[[1]][1]),scale = as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                         'beta' = pbeta(q=as.numeric(strsplit(input$borneschosed, ' ;')[[1]][2]), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2]))-pbeta(q=as.numeric(strsplit(input$borneschosed, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                         'cauchy' = pcauchy(q=as.numeric(strsplit(input$borneschosed, ' ;')[[1]][2]), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2]))-pcauchy(q=as.numeric(strsplit(input$borneschosed, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                         'asin' = repsin(x=as.numeric(strsplit(input$borneschosed, ' ;')[[1]][2]), a=as.numeric(strsplit(input$params, ' ;')[[1]][1]), b=as.numeric(strsplit(input$params, ' ;')[[1]][2]))-repsin(x=as.numeric(strsplit(input$borneschosed, ' ;')[[1]][1]), a=as.numeric(strsplit(input$params, ' ;')[[1]][1]), b=as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                         'weib' = pweibull(q=as.numeric(strsplit(input$borneschosed, ' ;')[[1]][2]), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2]))-pweibull(q=as.numeric(strsplit(input$borneschosed, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                         'gum' = repgumb(x=as.numeric(strsplit(input$borneschosed, ' ;')[[1]][2]), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2]))-repgumb(x=as.numeric(strsplit(input$borneschosed, ' ;')[[1]][1]), a=as.numeric(strsplit(input$params, ' ;')[[1]][1]), b=as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                         'lnorm' = dlnorm(as.numeric(strsplit(input$borneschosed, ' ;')[[1]][2]), as.numeric(strsplit(input$params, ' ;')[[1]][1]), sqrt(as.numeric(strsplit(input$params, ' ;')[[1]][2])))-dlnorm(q=as.numeric(strsplit(input$borneschosed, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][1]), sqrt(as.numeric(strsplit(input$params, ' ;')[[1]][2]))),
                         'khi' = dchisq(as.numeric(strsplit(input$borneschosed, ' ;')[[1]][2]),df = as.numeric(input$params))-dchisq(as.numeric(strsplit(input$borneschosed, ' ;')[[1]][1]),df = as.numeric(input$params)),
                         'stud' = dt(as.numeric(strsplit(input$borneschosed, ' ;')[[1]][2]),df = as.numeric(input$params))-dt(as.numeric(strsplit(input$borneschosed, ' ;')[[1]][1]),df = as.numeric(input$params)),
                         'par' = reppar(x=as.numeric(strsplit(input$borneschosed, ' ;')[[1]][2]), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2]))-reppar(x=as.numeric(strsplit(input$borneschosed, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                         'fish' = df(as.numeric(strsplit(input$borneschosed, ' ;')[[1]][2]), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2]))-df(as.numeric(strsplit(input$borneschosed, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2])),
                         'logi' = replogi(as.numeric(strsplit(input$borneschosed, ' ;')[[1]][2]), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2]))-replogi(as.numeric(strsplit(input$borneschosed, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][1]), as.numeric(strsplit(input$params, ' ;')[[1]][2])))
  ))
  
   output$calcul <- renderText({print(paste('By the power of Merlin, probability is mine to command ! The result muste be :',as.character(resultcalc()), ' !',sep=' '))})
  
   output$repartition <- renderUI({p({switch(input$law,'unif' = withMathJax('$$F(x)=\\dfrac{x-a}{b-a}\\textbf{1}\\{a\\leq x \\leq b\\}$$'),
                                       'norm' = withMathJax('$$F(x)=\\dfrac{1}{2}\\left(1+\\mathrm{erf}\\left(\\dfrac{x-\\mu}{\\sigma\\sqrt{x}}\\right)\\right)$$'),
                                       'exp' = withMathJax('$$F(x)=1- e^{-\\lambda x}\\textbf{1}_{\\mathbb{R}^+}$$'),
                                       'gamma' = withMathJax('$$F(x)=\\dfrac{\\gamma(k,\\dfrac{x}{\\theta})}{\\Gamma(k)}\\textbf{1}_{\\mathbb{R}^+},\\textrm{ with } \\gamma(a,x)=\\int_{0}^{x}t^{a-1}e^{-t}\\mathrm{d}t$$'),
                                       'beta' = withMathJax('$$F(x)=\\dfrac{\\textbf{B}_x(\\alpha,\\beta)}{\\textbf{B}(\\alpha,\\beta)} \\textbf{1}_{[0,1]},\\textrm{ with } \\textbf{B}_x(\\alpha,\\beta)=\\int_0^x t^{\\alpha-1}(1-t)^{\\beta-1}\\mathrm{d}t$$'),
                                       'asin' = withMathJax('$$F(x)=\\dfrac{2}{\\pi}\\arcsin\\left(\\sqrt{\\dfrac{(x-a)}{(b-x)}}\\right)\\textbf{1}_{[a,b]}$$'),
                                       'cauchy' = withMathJax('$$F(x)=\\dfrac{1}{\\pi}\\arctan\\left(\\dfrac{x-x_0}{a}\\right)+\\dfrac{1}{2}$$'),
                                       'weib' = withMathJax('$$F(x)=1-e^{-\\left(\\dfrac{x}{\\lambda}\\right)^k}\\textbf{1}_{\\mathbb{R}^+}$$'),
                                       'gum' = withMathJax('$$F(x)=\\mathrm{exp}\\left(-\\mathrm{exp}\\left(-\\dfrac{\\left(x-\\mu\\right)}{\\beta}\\right)\\right)$$'),
                                       'lnorm' = withMathJax('$$F(x)=\\dfrac{1}{2}+\\dfrac{1}{2}\\mathrm{erf}\\left(\\dfrac{\\log(x)-\\mu}{\\sigma\\sqrt{2}}\\right)\\textbf{1}_{\\mathbb{R}^+_*}$$'),
                                       'khi' = withMathJax('$$F(x)=\\dfrac{\\gamma\\left(\\dfrac{k}{2},\\dfrac{x}{2}\\right)}{\\Gamma\\left(\\frac{k}{2}\\right)}\\mathrm{1}_{\\mathbb{R}^+}\\ \\mathrm{with }\\ \\gamma(a,x)=\\int_0^xt^{a-1}e^{-t}\\mathrm{d}t$$'),
                                       'stud' = withMathJax('$$F(x)=\\dfrac{1}{2} +x\\Gamma\\left(\\dfrac{k+1}{2}\\right)\\dfrac{\\sideset{_2}{_1}F\\left(\\dfrac{1}{2},\\dfrac{k+1}{2},\\dfrac{3}{2},\\dfrac{-x^2}{k}\\right)}{\\sqrt{k\\pi}\\Gamma\\left(\\dfrac{k}{2}\\right)}\\ \\mathrm{with}\\ \\sideset{_2}{_1}F=\\sum_{n\\geq 0}\\dfrac{(a)_n(b)_n}{(c)_n}\\dfrac{z^n}{n!}\\ \\textrm{where}$$ $$\\ (\\alpha)_n\\ \\textrm{is the increasing factorial.} $$'),
                                       'par' = withMathJax('$$F(x)=1-\\left(\\dfrac{x_m}{x}\\right)^k\\textbf{1}_{[x_m,+\\infty[}$$'),
                                       'fish' = withMathJax('$$F(x)=\\textbf{I}_\\dfrac{d_1x}{}{x\\textbf{B}\\left(\\frac{d_1}{2},\\frac{d_2}{2}\\right)}\\textbf{1}_{\\mathbb{R}^+},\\textrm{ with }\\textbf{B}(x,y)=\\dfrac{\\Gamma(x)\\Gamma(y)}{\\Gamma(x+y)}$$'),
                                      'logi' = withMathJax('$$\\dfrac{e^{-\\frac{(x-\\mu)}{s}}}{s\\left(1+e^{-\\frac{(x-\\mu)}{s}}\\right)^2}$$'))})}) 
   my_list <- reactive({list(
     Convergence = c(`convergence presque sure` = 7, `convergence Lp` = 10, `convergence en loi` = 8, `convergence en probabilites` =9,
                     `Theoreme (lemme) de Borel-Cantelli` = 3,`Corollaire a fleures`=14, `Loi faible (et forte) des grands nombres` = 13,
                     `Theoreme de la limite centrale (TCL)` = 6, `Theoreme de Levy`= 15,
                     `Liens entre convergence ps et Lp`=32, `Liens entre convergence en probabilites en ps`=33,`Liens entre convergence Lp et probabilites`=34,
                     `Liens entre convergence en probabilites et en loi`=37),
     Inegalites = c(`Inegalite de Bienayme-Tchebychev` = 4, `Inegalite de Markov` = 2,`Inegalite de Holder`=22),
     Probas_Conditionnelles = c(`Probabilite conditionnelle`= 11,`Formule des probabilites totales` = 1, `Formule de Bayes` = 5),
     Fonction_Caracteristique = c(`Fonction caracteristique et generatrice` = 12,`Independance et fonctions caracteristiques`=31,`Inversion loi fonction caracteristique`=36),
     Bases = c(`Mesure image`=16,`Theoreme de Beppo-Levi`=19,`Theoreme de convergence dominee`=21,
               `Theoreme de Fatou`=20,`Esperance et moments`=23,`Theoreme de Caratheodory`=25, `Independance d'evenements`=17,
               `produit de convolution`=26,`Fonction de repartition`=30, `Loi absolument continue`=18,`independance de variables aleatoires`=24,
               `independance par blocs`=27,`covariance`=28, `coefficient de correlation`=29, `equi-integrabilite`=35)
   )})
   output$foo <- renderUI({selectizeInput('foo',options=list(placeholder='select one ressource please', maxItems=1), choices = my_list(),label='select',multiple=T)
     
   })
   
   output$theoreme <- renderUI({p(if(is.null(input$foo)){"No theorem chosed yet ! Don't be shy ! Like Alice said, it gets curiouser and curiouser..."}
                                  else {withMathJax(ids$enonce[as.numeric(input$foo)])})
                  })
}
    
# Run the application 
shinyApp(ui = ui, server = server)

##### Faire le profil (rentrer dans la console) -----------------
#profvis({ runApp(getwd()) }
#, prof_output = getwd())
#profvis(prof_input = '~/programmation/R/shiny/loisswitch/app/profile.Rprof')
#