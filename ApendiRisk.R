#-----------------------------------------------------------------
# Sistema de evaluación de apendicitis
# Versión con modelo de regresión logística y almacenamiento CSV
# Base: código de interfaz corregido por el equipo
#-----------------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(dplyr)
library(gridExtra)
library(grid)


# =========================================================
# RUTA DEL CSV TEMPORAL (persiste dentro de la sesión de R)
# Cambiá esta ruta si querés un directorio fijo
# =========================================================
CSV_PATH <- file.path(tempdir(), "pacientes_apendicitis.csv")


# =========================================================
# COEFICIENTES DEL MODELO DE REGRESIÓN LOGÍSTICA
# Fuente: análisis propio del equipo
# =========================================================
COEF <- list(
  intercepto  = -122.2,
  duracion    =   0.0030,
  temperatura =   1.760,
  rebote      =   2.477,
  mcburney    =   2.978,
  rovsing     =   1.706,
  psoas       =   2.345,
  wbc         =   0.947,   # WBC en miles (k/uL)
  neutrofilos =   0.5260,
  crp         =   0.0929
)

# Umbrales de probabilidad para clasificación
UMBRAL_ROJO     <- 0.70   # >= 70% → alta sospecha
UMBRAL_AMARILLO <- 0.35   # >= 35% → sospecha intermedia
                          # <  35% → baja sospecha


# =========================================================
# FUNCIÓN: calcular probabilidad con el modelo logístico
# =========================================================
calcular_probabilidad <- function(duracion, temperatura, rebote,
                                  mcburney, rovsing, psoas,
                                  wbc_ul, neutrofilos, crp) {

  # Variables dummy (Sí=1, No=0)
  r  <- ifelse(rebote   == "Sí", 1, 0)
  m  <- ifelse(mcburney == "Sí", 1, 0)
  ro <- ifelse(rovsing  == "Sí", 1, 0)
  ps <- ifelse(psoas    == "Sí", 1, 0)

  # WBC viene en /uL desde la UI; el modelo usa k/uL
  wbc_k <- wbc_ul / 1000

  logit <- COEF$intercepto +
    COEF$duracion    * duracion    +
    COEF$temperatura * temperatura +
    COEF$rebote      * r           +
    COEF$mcburney    * m           +
    COEF$rovsing     * ro          +
    COEF$psoas       * ps          +
    COEF$wbc         * wbc_k       +
    COEF$neutrofilos * neutrofilos +
    COEF$crp         * crp

  prob <- 1 / (1 + exp(-logit))
  return(round(prob, 4))
}


# =========================================================
# FUNCIÓN: clasificar según probabilidad
# =========================================================
clasificar_prioridad <- function(prob) {
  if (prob >= UMBRAL_ROJO) {
    "rojo"
  } else if (prob >= UMBRAL_AMARILLO) {
    "amarillo"
  } else {
    "verde"
  }
}


# =========================================================
# FUNCIONES: guardar / cargar CSV
# =========================================================
guardar_csv <- function(df) {
  write.csv(df, CSV_PATH, row.names = FALSE)
}

cargar_csv <- function() {
  if (file.exists(CSV_PATH)) {
    tryCatch(
      read.csv(CSV_PATH, stringsAsFactors = FALSE),
      error = function(e) data.frame()
    )
  } else {
    data.frame()
  }
}


#-----------------------------------------------------------------
# Interfaz de usuario
#-----------------------------------------------------------------

ui <- fluidPage(


  tags$style(HTML("

    .titulo{
      text-align:center;
      font-size:32px;
      font-weight:bold;
      color:#2C3E50;
    }

    .gradient{
      height:45px;
      width:100%;
      border-radius:20px;
      background:linear-gradient(
        to right,
        #2ecc71,
        #f1c40f,
        #e74c3c
      );
      margin-top:25px;
      margin-bottom:30px;
    }

    .danger-box{
      background:#e74c3c;
      color:white;
      padding:25px;
      border-radius:15px;
      text-align:center;
      font-size:20px;
      font-weight:bold;
      margin-bottom:15px;
    }

    .warning-box{
      background:#f1c40f;
      color:black;
      padding:25px;
      border-radius:15px;
      text-align:center;
      font-size:20px;
      font-weight:bold;
      margin-bottom:15px;
    }

    .safe-box{
      background:#2ecc71;
      color:white;
      padding:25px;
      border-radius:15px;
      text-align:center;
      font-size:20px;
      font-weight:bold;
      margin-bottom:15px;
    }

  ")),


  navbarPage(

    "Sistema de evaluación de apendicitis",


    # ----------------------------------------------------------
    # TAB 1: Datos del paciente
    # ----------------------------------------------------------
    tabPanel(
      "Datos del paciente",

      sidebarLayout(

        sidebarPanel(

          h4("Datos básicos"),

          textInput(
            "nombre",
            "Nombre del paciente"
          ),

          selectInput(
            "genero",
            "Género",
            choices = c("Masculino", "Femenino")
          ),

          numericInput(
            "edad",
            "Edad",
            value = NULL,
            min = 0
          ),

          conditionalPanel(
            "input.genero == 'Femenino'",

            radioButtons(
              "embarazo",
              "¿Embarazo?",
              choices = c("Sí", "No")
            )
          ),

          hr(),

          h4("Síntomas y signos"),

          numericInput(
            "duracion",
            "Duración síntomas (horas)",
            value = NULL,
            min = 0
          ),

          numericInput(
            "temp",
            "Temperatura (°C)",
            value = NULL,
            min = 0
          ),

          selectInput(
            "rebote",
            "Sensibilidad de rebote",
            choices = c("Sí", "No")
          ),

          selectInput(
            "mcburney",
            "Signo de McBurney",
            choices = c("Sí", "No")
          ),

          selectInput(
            "rovsing",
            "Signo de Rovsing",
            choices = c("Sí", "No")
          ),

          selectInput(
            "psoas",
            "Signo del Psoas",
            choices = c("Sí", "No")
          ),

          hr(),

          h4("Exámenes realizados"),

          numericInput(
            "wbc",
            "Recuento leucocitos (/uL)",
            value = NULL,
            min = 0
          ),

          numericInput(
            "neutro",
            "Neutrófilos (%)",
            value = NULL,
            min = 0
          ),

          numericInput(
            "crp",
            "Proteína C reactiva (CRP)",
            value = NULL,
            min = 0
          ),

          hr(),

          actionButton(
            "guardar",
            "Guardar datos",
            class = "btn-primary"
          ),

          br(), br(),

          downloadButton(
            "descargar_csv",
            "Descargar CSV de pacientes",
            class = "btn-default",
            style = "width:100%"
          )

        ),


        mainPanel(
          h3("Pacientes registrados"),
          tableOutput("tabla")
        )

      )
    ),


    # ----------------------------------------------------------
    # TAB 2: Prioridad de atención
    # ----------------------------------------------------------
    tabPanel(
      "Prioridad de atención",

      br(),

      fluidRow(

        column(
          width = 9,
          h2(
            "PRIORIDAD DE ATENCIÓN",
            class = "titulo"
          )
        ),

        column(
          width = 3,
          align = "right",

          downloadButton(
            "exportar",
            "Exportar PDF",
            class = "btn-success"
          )
        )

      ),


      div(class = "gradient"),


      selectInput(
        "paciente_pdf",
        "Seleccionar paciente:",
        choices = NULL
      ),

      uiOutput("nivel")

    )

  )

)


#-----------------------------------------------------------------
# Servidor
#-----------------------------------------------------------------

server <- function(input, output, session) {


  # Inicializa desde CSV si ya existe (persiste entre recargas
  # dentro de la misma sesión de R)
  datos <- reactiveVal(cargar_csv())


  # ----------------------------------------------------------
  # Guardar nuevo paciente
  # ----------------------------------------------------------
  observeEvent(input$guardar, {


    # --- Validación de campos numéricos obligatorios ---
    campos_numericos <- list(
      Edad        = input$edad,
      Duracion    = input$duracion,
      Temperatura = input$temp,
      WBC         = input$wbc,
      Neutrofilos = input$neutro,
      CRP         = input$crp
    )

    vacios <- sapply(
      campos_numericos,
      function(x) is.null(x) || length(x) == 0 || is.na(x)
    )

    if (any(vacios)) {

      showNotification(
        paste(
          "Completa estos campos numéricos antes de guardar:",
          paste(names(campos_numericos)[vacios], collapse = ", ")
        ),
        type = "error"
      )

      return()

    }


    # --- Calcular probabilidad con el modelo logístico ---
    prob <- calcular_probabilidad(
      duracion    = input$duracion,
      temperatura = input$temp,
      rebote      = input$rebote,
      mcburney    = input$mcburney,
      rovsing     = input$rovsing,
      psoas       = input$psoas,
      wbc_ul      = input$wbc,
      neutrofilos = input$neutro,
      crp         = input$crp
    )

    prioridad <- clasificar_prioridad(prob)


    # --- Construir fila del nuevo paciente ---
    nuevo <- data.frame(

      ID          = nrow(datos()) + 1,

      Nombre      = input$nombre,
      Genero      = input$genero,
      Edad        = input$edad,

      Embarazo    = ifelse(
        input$genero == "Femenino",
        input$embarazo,
        NA
      ),

      Duracion    = input$duracion,
      Temperatura = input$temp,

      Rebote      = input$rebote,
      McBurney    = input$mcburney,
      Rovsing     = input$rovsing,
      Psoas       = input$psoas,

      WBC         = input$wbc,
      Neutrofilos = input$neutro,
      CRP         = input$crp,

      Probabilidad = prob,
      Prioridad    = prioridad,

      stringsAsFactors = FALSE

    )


    # --- Actualizar reactive y guardar en CSV ---
    df_actualizado <- rbind(datos(), nuevo)
    datos(df_actualizado)
    guardar_csv(df_actualizado)

    showNotification(
      paste0(
        "✔ Paciente guardado. Probabilidad de apendicitis: ",
        round(prob * 100, 1), "%"
      ),
      type = "message",
      duration = 4
    )


    # --- Limpiar el formulario para el siguiente paciente ---
    updateTextInput(session, "nombre",   value = "")
    updateSelectInput(session, "genero", selected = "Masculino")
    updateNumericInput(session, "edad",  value = NA)
    updateRadioButtons(session, "embarazo", selected = "Sí")

    updateNumericInput(session, "duracion", value = NA)
    updateNumericInput(session, "temp",     value = NA)
    updateSelectInput(session, "rebote",    selected = "Sí")
    updateSelectInput(session, "mcburney",  selected = "Sí")
    updateSelectInput(session, "rovsing",   selected = "Sí")
    updateSelectInput(session, "psoas",     selected = "Sí")

    updateNumericInput(session, "wbc",   value = NA)
    updateNumericInput(session, "neutro", value = NA)
    updateNumericInput(session, "crp",   value = NA)


  })


  # ----------------------------------------------------------
  # Actualizar selector de paciente
  # ----------------------------------------------------------
  observe({

    updateSelectInput(
      session,
      "paciente_pdf",
      choices = setNames(
        datos()$ID,
        datos()$Nombre
      )
    )

  })


  # ----------------------------------------------------------
  # Tabla de pacientes registrados
  # ----------------------------------------------------------
  output$tabla <- renderTable({
    datos()
  })


  # ----------------------------------------------------------
  # Descargar CSV desde la UI
  # ----------------------------------------------------------
  output$descargar_csv <- downloadHandler(
    filename = function() {
      paste0("pacientes_apendicitis_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(datos(), file, row.names = FALSE)
    }
  )


  # ----------------------------------------------------------
  # Panel de prioridad: muestra solo el paciente seleccionado
  # (corrección de tu compañera: comparación con as.character)
  # ----------------------------------------------------------
  output$nivel <- renderUI({

    req(nrow(datos()) > 0)
    req(input$paciente_pdf)

    paciente <- datos()[
      as.character(datos()$ID) == as.character(input$paciente_pdf),
    ]

    req(nrow(paciente) == 1)

    prob_pct <- paste0(round(paciente$Probabilidad * 100, 1), "%")

    if (paciente$Prioridad == "rojo") {

      div(
        class = "danger-box",
        paste("🔴", paciente$Nombre, "- ATENCIÓN URGENTE POR APENDICITIS"),
        br(),
        tags$span(
          style = "font-size:14px; font-weight:normal;",
          paste("Probabilidad estimada:", prob_pct)
        )
      )

    } else if (paciente$Prioridad == "amarillo") {

      div(
        class = "warning-box",
        paste("🟡", paciente$Nombre, "- Evaluación complementaria"),
        br(),
        tags$span(
          style = "font-size:14px; font-weight:normal;",
          paste("Probabilidad estimada:", prob_pct)
        )
      )

    } else {

      div(
        class = "safe-box",
        paste("🟢", paciente$Nombre, "- Baja sospecha / seguimiento"),
        br(),
        tags$span(
          style = "font-size:14px; font-weight:normal;",
          paste("Probabilidad estimada:", prob_pct)
        )
      )

    }

  })


  # ----------------------------------------------------------
  # Exportar PDF
  # (código original de la compañera + probabilidad agregada)
  # ----------------------------------------------------------
  output$exportar <- downloadHandler(

    filename = function() {
      paste0(
        "Caso_clinico_apendicitis_",
        Sys.Date(),
        ".pdf"
      )
    },

    content = function(file) {


      req(input$paciente_pdf)

      paciente <- datos()[
        datos()$ID == as.integer(input$paciente_pdf),
      ]

      req(nrow(paciente) == 1)

      prioridad <- paciente$Prioridad
      prob_pct  <- paste0(round(paciente$Probabilidad * 100, 1), "%")

      interpretacion <- if (prioridad == "rojo") {

        paste0(
          "ALTA PRIORIDAD: Probabilidad de apendicitis ", prob_pct, ".\n",
          "Sospecha elevada. Requiere valoración médica urgente."
        )

      } else if (prioridad == "amarillo") {

        paste0(
          "PRIORIDAD INTERMEDIA: Probabilidad de apendicitis ", prob_pct, ".\n",
          "Requiere estudios complementarios\ny descarte de diagnósticos diferenciales."
        )

      } else {

        paste0(
          "BAJA PRIORIDAD: Probabilidad de apendicitis ", prob_pct, ".\n",
          "Baja sospecha actual.\nMantener seguimiento clínico."
        )

      }


      color <- if (prioridad == "rojo") {
        "#e74c3c"
      } else if (prioridad == "amarillo") {
        "#f1c40f"
      } else {
        "#2ecc71"
      }

      color_texto <- if (prioridad == "amarillo") {
        "black"
      } else {
        "white"
      }


      pdf(
        file,
        width  = 8.5,
        height = 11
      )

      # IMPORTANTE: no se llama grid.newpage() aquí.
      # grid.arrange() ya abre una página nueva por defecto
      # (newpage=TRUE). Llamarlo dos veces generaba la página
      # en blanco al inicio del PDF.


      # Tema de tabla: columna izquierda en negrita con fondo gris,
      # columna derecha en blanco, ambas con borde

      tema_reporte <- ttheme_default(

        core = list(
          fg_params = list(hjust = 0, x = 0.05, fontsize = 10),
          bg_params = list(fill = "white", col = "grey50", lwd = 1)
        ),

        rowhead = list(
          fg_params = list(hjust = 0, x = 0.05, fontface = "bold",
                           fontsize = 10, col = "#2C3E50"),
          bg_params = list(fill = "#ECF0F1", col = "grey50", lwd = 1)
        )

      )


      # Encabezado del reporte

      titulo <- textGrob(
        "REPORTE DE EVALUACIÓN - APENDICITIS",
        gp = gpar(fontsize = 20, fontface = "bold", col = "#2C3E50")
      )

      subtitulo <- textGrob(
        paste0(
          "Paciente: ", paciente$Nombre,
          "   |   Fecha de generación: ", format(Sys.Date(), "%d/%m/%Y")
        ),
        gp = gpar(fontsize = 10, col = "grey30")
      )


      # Barra de sección con fondo oscuro

      seccion <- function(texto) {

        grobTree(
          rectGrob(gp = gpar(fill = "#2C3E50", col = NA)),
          textGrob(
            texto,
            x = 0.02, just = "left",
            gp = gpar(fontsize = 12, fontface = "bold", col = "white")
          )
        )

      }


      # Tabla: datos del paciente

      datos_personales <- tableGrob(
        data.frame(
          Resultado = c(
            paciente$Nombre,
            paciente$Genero,
            paste(paciente$Edad, "años"),
            ifelse(
              is.na(paciente$Embarazo),
              "No aplica",
              paciente$Embarazo
            )
          )
        ),
        rows  = c("Nombre", "Género", "Edad", "Embarazo"),
        cols  = NULL,
        theme = tema_reporte
      )


      # Tabla: signos y síntomas

      signos <- tableGrob(
        data.frame(
          Resultado = c(
            paste(paciente$Duracion, "horas"),
            paste(paciente$Temperatura, "°C"),
            paciente$Rebote,
            paciente$McBurney,
            paciente$Rovsing,
            paciente$Psoas
          )
        ),
        rows = c(
          "Duración de síntomas",
          "Temperatura",
          "Sensibilidad de rebote",
          "Signo de McBurney",
          "Signo de Rovsing",
          "Signo del Psoas"
        ),
        cols  = NULL,
        theme = tema_reporte
      )


      # Tabla: exámenes de laboratorio

      laboratorio <- tableGrob(
        data.frame(
          Resultado = c(
            paciente$WBC,
            paciente$Neutrofilos,
            paciente$CRP
          )
        ),
        rows = c(
          "Leucocitos (WBC)",
          "Neutrófilos (%)",
          "Proteína C reactiva (CRP)"
        ),
        cols  = NULL,
        theme = tema_reporte
      )


      # Caja de prioridad con probabilidad incluida

      prioridad_box <- grobTree(

        rectGrob(gp = gpar(fill = color, col = "black", lwd = 1.5)),

        textGrob(
          paste0(
            "PRIORIDAD DE ATENCIÓN: ", toupper(prioridad),
            "   |   Probabilidad estimada: ", prob_pct
          ),
          gp = gpar(fontsize = 13, fontface = "bold", col = color_texto)
        )

      )


      interpretacion_grob <- textGrob(
        interpretacion,
        x    = 0.02,
        just = "left",
        gp   = gpar(fontsize = 10)
      )

      nota_modelo <- textGrob(
        "* Probabilidad calculada con modelo de regresión logística validado internamente.",
        x    = 0.02,
        just = "left",
        gp   = gpar(fontsize = 8, col = "grey40")
      )


      grid.arrange(

        titulo,
        subtitulo,

        seccion("DATOS DEL PACIENTE"),
        datos_personales,

        seccion("SIGNOS Y SÍNTOMAS"),
        signos,

        seccion("EXÁMENES DE LABORATORIO"),
        laboratorio,

        seccion("PRIORIDAD DE ATENCIÓN"),
        prioridad_box,
        interpretacion_grob,
        nota_modelo,

        ncol = 1,

        heights = c(
          0.5,   # título
          0.3,   # subtítulo
          0.35,  # sección 1
          1.3,   # tabla personal
          0.35,  # sección 2
          1.9,   # tabla signos
          0.35,  # sección 3
          1.0,   # tabla laboratorio
          0.35,  # sección 4
          0.6,   # caja prioridad
          0.7,   # interpretación
          0.3    # nota modelo
        )

      )


      dev.off()

    }

  )


}


shinyApp(ui, server)
