#shiny app

ui <- fluidPage(titlePanel("Painel das chances de mortalidade por câncer no Brasil."),
                sidebarLayout(sidebarPanel(helpText(h3("Selecione o sexo da parcela da população brasileira que deseja estudar"))
                                           ,selectInput("sex","Sexo",c("Masculino"="masculino","Feminino"="feminino")),selectInput("reg","Região",c("Norte"="norte","Nordeste"="nordeste","Sudeste"="sudeste","Sul"="sul","Centro Oeste"="centro oeste")))
                              ,mainPanel(h2("MOR",align="center"),
                                         p("Aplicativo shiny realizado para monitorar a MOR por região da população brasileira por câncer(calculada no período entre 1985 até 2018)."),textOutput("selected_var"),textOutput("new_selected_var"),plotOutput("myplot"),textOutput("faixa"),
                                         tableOutput("morh")
                                         
                              )
                )
)



