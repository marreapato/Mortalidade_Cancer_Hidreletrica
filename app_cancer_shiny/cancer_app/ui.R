#shiny app

ui <- fluidPage(titlePanel("Painel das chances de mortalidade por câncer no Brasil."),
                sidebarLayout(sidebarPanel(helpText(h3("Selecione o sexo da parcela da população brasileira que deseja estudar"))
                                           ,selectInput("sex","Sexo",c("Masculino"="masculino","Feminino"="feminino")),selectInput("reg","Região",c("Norte"="norte","Nordeste"="nordeste","Sudeste"="sudeste","Sul"="sul","Centro Oeste"="centro oeste")),helpText(h5("Como usar: Quanto maior o valor presente na faixa etária de um grupo maior será sua chance de óbito por câncer em relação ao outro considerado. Por exemplo: Indivíduos do sexo masculino de 00 a 04 anos, residentes na região norte tiveram a maior chance de óbito em relação as outros do mesmo sexo, no período de 1985 a 2018.")))
                              ,mainPanel(h2("MOR",align="center"),
                                         p("Aplicativo shiny realizado para monitorar a MOR por região da população brasileira por câncer(calculada no período entre 1985 até 2018)."),textOutput("selected_var"),textOutput("new_selected_var"),plotOutput("myplot"),textOutput("faixa"),
                                         tableOutput("morh")
                                         
                              )
                )
)



