# app.R — wrapper para Posit Connect
# Carga definiciones (ui, server) en el entorno actual
source("Demo_Pina.R", local = TRUE)

# Devuelve el shiny.appobj que Connect espera
shinyApp(ui = ui, server = server)
