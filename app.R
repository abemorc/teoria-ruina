library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(scales)

# ==============================================================================
# 1. MOTOR MATEMÁTICO Y DE SIMULACIÓN (TEORÍA DE LA RUINA EN TIEMPO DISCRETO)
# ==============================================================================

obtener_distribucion <- function(modelo, params) {
  if (modelo == "ej1") {
    # Ejemplo 7.1 / 1.1: Distribución discreta finita en {0, 1, 2, 3}
    probs <- c(params$f0, params$f1, params$f2, params$f3)
    probs[is.na(probs) | probs < 0] <- 0
    if (sum(probs) == 0) probs <- c(0.5, 0.2, 0.3, 0)
    probs <- probs / sum(probs)
    
    y_vals <- 0:3
    f_y <- probs
    F_y <- cumsum(f_y)
    F_bar <- pmax(0, 1 - F_y)
    EY <- sum(y_vals * f_y)
    
    y_ext <- 0:200
    f_ext <- c(f_y, rep(0, length(y_ext) - length(f_y)))
    F_bar_ext <- pmax(0, 1 - cumsum(f_ext))
    
  } else if (modelo == "ej2") {
    # Ejemplo 7.2 / 1.2: Ruina del jugador Y en {0, 2}
    p <- params$p_jugador
    y_vals <- 0:2
    f_y <- c(p, 0, 1 - p)
    F_y <- cumsum(f_y)
    F_bar <- pmax(0, 1 - F_y)
    EY <- 2 * (1 - p)
    
    y_ext <- 0:200
    f_ext <- c(f_y, rep(0, length(y_ext) - length(f_y)))
    F_bar_ext <- pmax(0, 1 - cumsum(f_ext))
    
  } else {
    # Ejemplo 7.3: Reclamaciones geométricas f(y) = (1 - p)^y * p, y = 0, 1, 2...
    p <- params$p_geom
    y_ext <- 0:500
    f_ext <- dgeom(y_ext, prob = p)
    F_bar_ext <- (1 - p)^(y_ext + 1)
    EY <- (1 - p) / p
    
    y_vals <- 0:15
    f_y <- f_ext[1:16]
    F_y <- cumsum(f_y)
    F_bar <- F_bar_ext[1:16]
  }
  
  list(
    y_tabla = y_vals,
    f_tabla = f_y,
    F_tabla = F_y,
    F_bar_tabla = F_bar,
    y_ext = y_ext,
    f_ext = f_ext,
    F_bar_ext = F_bar_ext,
    EY = EY
  )
}

calcular_psi_recursivo <- function(dist, u_max, modelo, params) {
  EY <- dist$EY
  f0 <- dist$f_ext[1]
  F_bar <- dist$F_bar_ext
  u_vec <- 0:u_max
  psi <- numeric(length(u_vec))
  
  if (EY >= 1 - 1e-12) {
    if (abs(dist$f_ext[2] - 1) < 1e-12) {
      psi <- ifelse(u_vec == 0, 1, 0)
    } else {
      psi <- rep(1, length(u_vec))
    }
    return(data.frame(u = u_vec, psi_recursivo = psi, psi_cerrada = psi))
  }
  
  psi[1] <- EY
  if (u_max >= 1) {
    for (u in 1:u_max) {
      suma_cola <- sum(F_bar[(u + 1):length(F_bar)])
      suma_conv <- 0
      if (u >= 2) {
        for (y in 1:(u - 1)) {
          suma_conv <- suma_conv + psi[u - y + 1] * F_bar[y + 1]
        }
      }
      psi[u + 1] <- (suma_cola + suma_conv) / f0
    }
  }
  
  psi_cerrada <- rep(NA_real_, length(u_vec))
  if (modelo == "ej2") {
    p <- params$p_jugador
    psi_cerrada[1] <- 2 * (1 - p)
    if (u_max >= 1) {
      psi_cerrada[2:(u_max + 1)] <- ((1 - p) / p)^(1:u_max)
    }
  } else if (modelo == "ej3") {
    p <- params$p_geom
    psi_cerrada <- ((1 - p) / p)^(u_vec + 1)
  }
  
  data.frame(
    u = u_vec,
    psi_recursivo = pmin(1, pmax(0, psi)),
    psi_cerrada = psi_cerrada
  )
}

simular_Y <- function(n_total, modelo, params, dist) {
  if (modelo == "ej1") {
    sample(0:3, size = n_total, replace = TRUE, prob = dist$f_tabla)
  } else if (modelo == "ej2") {
    p <- params$p_jugador
    ifelse(runif(n_total) < p, 0L, 2L)
  } else {
    p <- params$p_geom
    rgeom(n_total, prob = p)
  }
}

generar_trayectoria_individual <- function(u0, N, modelo, params, dist, forzar_ruina = FALSE) {
  mejor_Y <- NULL
  max_intentos <- if (forzar_ruina) 500 else 1
  
  for (intento in 1:max_intentos) {
    Y_vec <- simular_Y(N, modelo, params, dist)
    C_vec <- c(u0, u0 + cumsum(1 - Y_vec))
    if (!forzar_ruina || any(C_vec[-1] <= 0)) {
      mejor_Y <- Y_vec
      break
    }
    mejor_Y <- Y_vec
  }
  
  C_vec <- c(u0, u0 + cumsum(1 - mejor_Y))
  idx_ruina <- which(C_vec[-1] <= 0)
  tau <- if (length(idx_ruina) > 0) idx_ruina[1] else NA_integer_
  
  list(
    u0 = u0,
    N = N,
    Y = mejor_Y,
    Y_acum = c(0, cumsum(mejor_Y)),
    C = C_vec,
    tau = tau
  )
}

# ==============================================================================
# 2. INTERFAZ DE USUARIO (DOS VISTAS LIMPIAS + CONTROL JS DE BAJA LATENCIA)
# ==============================================================================

ui <- page_navbar(
  id = "nav_principal",
  title = "Teoría de la Ruina en Tiempo Discreto",
  fillable = FALSE,
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B4F72",
    secondary = "#5D6D7E"
  ),
  
  header = tagList(
    withMathJax(),
    tags$head(
      tags$style(HTML("
        .bslib-value-box {
          min-height: 92px !important;
          max-height: 114px !important;
        }
        .bslib-value-box .value-box-showcase {
          font-size: 1.8rem !important;
          padding: 0.45rem !important;
        }
        .bslib-value-box .value-box-title {
          font-size: 0.85rem !important;
          margin-bottom: 0.15rem !important;
        }
        .bslib-value-box .value-box-value {
          font-size: 1.4rem !important;
          line-height: 1.15 !important;
        }
        .telemetry-card {
          border-radius: 8px;
          padding: 10px 12px;
          text-align: center;
          border: 1px solid #D5D8DC;
          background-color: #F8F9F9;
        }
        .telemetry-label {
          font-size: 0.78rem;
          color: #566573;
          font-weight: 600;
          margin-bottom: 2px;
        }
        .telemetry-val {
          font-size: 1.25rem;
          font-weight: 700;
          color: #1B4F72;
        }
        .telemetry-ruina {
          background-color: #FADBD8 !important;
          border-color: #C0392B !important;
        }
        .telemetry-ruina .telemetry-val {
          color: #922B21 !important;
        }
        .narrative-box {
          min-height: 54px;
          display: flex;
          align-items: center;
          margin-bottom: 10px !important;
        }
      ")),
      # Script cliente para reproducción sincronizada y pausa instantánea (0 ms)
      tags$script(HTML("
        (function() {
          let isPlaying = false;
          let timerId = null;
          let currentDelay = 1500;
          let tickCounter = 0;

          function clearAnimTimer() {
            if (timerId !== null) {
              clearTimeout(timerId);
              timerId = null;
            }
          }

          function sendTick() {
            if (!isPlaying) return;
            tickCounter += 1;
            Shiny.setInputValue('anim_tick', tickCounter, {priority: 'event'});
          }

          function updateButtonVisual(playing) {
            const btn = document.getElementById('btn_play_pause');
            if (!btn) return;
            if (playing) {
              btn.classList.remove('btn-success');
              btn.classList.add('btn-warning');
              btn.innerHTML = '<i class=\"fa fa-pause\"></i> Pausar';
            } else {
              btn.classList.remove('btn-warning');
              btn.classList.add('btn-success');
              btn.innerHTML = '<i class=\"fa fa-play\"></i> Reproducir';
            }
          }

          Shiny.addCustomMessageHandler('control_anim', function(msg) {
            clearAnimTimer();
            if (typeof msg.delay === 'number') {
              currentDelay = msg.delay;
            }
            isPlaying = !!msg.playing;
            updateButtonVisual(isPlaying);
            if (isPlaying && msg.step_now) {
              sendTick();
            }
          });

          // Pausa inmediata en el navegador apenas se hace clic si estaba reproduciendo
          document.addEventListener('click', function(e) {
            const btn = e.target.closest('#btn_play_pause');
            if (btn && isPlaying) {
              isPlaying = false;
              clearAnimTimer();
              updateButtonVisual(false);
            }
          }, true);

          // Agenda el siguiente paso SOLO después de que la gráfica actual terminó de pintarse
          $(document).on('shiny:value', function(e) {
            if (e.name === 'plot_animacion_unica' && isPlaying) {
              clearAnimTimer();
              timerId = setTimeout(sendTick, currentDelay);
            }
          });

          // Actualiza el intervalo al instante si el usuario mueve el deslizador de segundos
          $(document).on('shiny:inputchanged', function(e) {
            if (e.name === 'vel_anim') {
              currentDelay = Number(e.value) * 1000;
              if (isPlaying && timerId !== null) {
                clearAnimTimer();
                timerId = setTimeout(sendTick, currentDelay);
              }
            }
          });
        })();
      "))
    )
  ),
  
  sidebar = sidebar(
    width = 320,
    title = "Parámetros del Proceso",
    
    selectInput(
      "modelo",
      "1. Distribución de reclamaciones (Y):",
      choices = c(
        "Ejemplo 7.1: Discreta Tabulada" = "ej1",
        "Ejemplo 7.2: Ruina del Jugador (0 o 2)" = "ej2",
        "Ejemplo 7.3: Reclamaciones Geométricas" = "ej3"
      ),
      selected = "ej1"
    ),
    
    conditionalPanel(
      condition = "input.modelo == 'ej1'",
      helpText("Probabilidades puntuales \\(f(y) = P(Y = y)\\):"),
      layout_columns(
        col_widths = c(6, 6),
        numericInput("f0", "f(0):", value = 0.5, min = 0, max = 1, step = 0.05),
        numericInput("f1", "f(1):", value = 0.2, min = 0, max = 1, step = 0.05),
        numericInput("f2", "f(2):", value = 0.3, min = 0, max = 1, step = 0.05),
        numericInput("f3", "f(3):", value = 0.0, min = 0, max = 1, step = 0.05)
      ),
      uiOutput("aviso_suma_probs"),
      actionButton("reset_ej1", "Restaurar tabla del libro", class = "btn-sm btn-outline-secondary w-100 mb-2")
    ),
    
    conditionalPanel(
      condition = "input.modelo == 'ej2'",
      sliderInput(
        "p_jugador",
        "Probabilidad p = P(Y = 0):",
        min = 0.10, max = 0.95, value = 0.65, step = 0.01
      ),
      helpText("Ganancia neta: \\(p > 0.50 \\iff E[Y] = 2(1-p) < 1\\).")
    ),
    
    conditionalPanel(
      condition = "input.modelo == 'ej3'",
      sliderInput(
        "p_geom",
        "Parámetro geométrico p:",
        min = 0.10, max = 0.95, value = 0.60, step = 0.01
      ),
      helpText("Ganancia neta: \\(p > 0.50 \\iff E[Y] = \\frac{1-p}{p} < 1\\).")
    ),
    
    hr(),
    sliderInput("u_inicial", "2. Capital inicial (u):", min = 0, max = 15, value = 3, step = 1),
    sliderInput("horizonte_N", "3. Horizonte de tiempo (N):", min = 15, max = 120, value = 35, step = 5),
    
    conditionalPanel(
      condition = "input.nav_principal === 'lab'",
      hr(),
      h6("4. Controles de Monte Carlo y Tablas"),
      sliderInput("u_max_tabla", "Capital máximo en tablas/gráficas:", min = 5, max = 20, value = 10, step = 1),
      sliderInput("n_trayectorias", "Trayectorias en Pestaña 1:", min = 5, max = 35, value = 15, step = 1),
      sliderInput("n_sim", "Carteras simuladas:", min = 500, max = 10000, value = 3000, step = 500),
      actionButton("resimular", "Nuevas simulaciones", icon = icon("dice"), class = "btn-primary w-100 mt-2")
    )
  ),
  
  # ============================================================================
  # VISTA 1: INTRODUCCIÓN VISUAL AL PROBLEMA (SIN DISTRACCIONES DE MONTE CARLO)
  # ============================================================================
  nav_panel(
    title = "1. Introducción al Problema",
    value = "intro",
    icon = icon("person-chalkboard"),
    
    card(
      card_body(
        div(
          class = "bg-light border rounded p-2 mb-3 text-center",
          HTML("<b>Proceso de riesgo:</b> \\(C_n = u + n - \\sum_{j=1}^n Y_j\\) &emsp;|&emsp; 
                <b>Tiempo de ruina:</b> \\(\\tau = \\min\\{n \\ge 1 : C_n \\le 0\\}\\)")
        ),
        
        layout_columns(
          col_widths = c(3, 2, 2, 2, 3),
          actionButton("btn_play_pause", "Reproducir", icon = icon("play"), class = "btn-success w-100 fw-bold"),
          actionButton("btn_step", "Paso +1", icon = icon("forward-step"), class = "btn-outline-primary w-100"),
          actionButton("btn_reset_paso", "Reiniciar (n=0)", icon = icon("backward-fast"), class = "btn-outline-secondary w-100"),
          actionButton("btn_nueva_traj", "Nueva ruta", icon = icon("shuffle"), class = "btn-outline-dark w-100"),
          actionButton(
            "btn_forzar_ruina",
            "Ruta con ruina",
            icon = icon("burst"),
            class = "btn-outline-danger w-100",
            title = "Mantiene la misma distribución de Y, pero busca una realización donde ocurra la ruina"
          )
        ),
        
        layout_columns(
          col_widths = c(7, 5),
          sliderInput("slider_paso", "Explorar periodo (n):", min = 0, max = 35, value = 0, step = 1),
          sliderInput("vel_anim", "Tiempo entre pasos (segundos):", min = 0.5, max = 10, value = 1.5, step = 0.5)
        ),
        
        uiOutput("telemetria_trayectoria"),
        uiOutput("estado_paso_actual"),
        plotOutput("plot_animacion_unica", height = "460px")
      )
    )
  ),
  
  # ============================================================================
  # VISTA 2: LABORATORIO Y VALIDACIÓN TEÓRICA (POST-PIZARRÓN)
  # ============================================================================
  nav_panel(
    title = "2. Laboratorio y Validación Teórica",
    value = "lab",
    icon = icon("chart-bar"),
    
    uiOutput("kpi_boxes"),
    
    navset_card_underline(
      id = "tabs_laboratorio",
      
      nav_panel(
        title = "1. Múltiples Trayectorias",
        card_body(
          p(HTML("Evolución simultánea de múltiples realizaciones de \\(C_n\\). 
                 Las trayectorias en <span style='color:#C0392B; font-weight:bold;'>rojo</span> sufren ruina en el primer instante 
                 \\(\\tau = \\min\\{n \\ge 1 : C_n \\le 0\\}\\).")),
          plotOutput("plot_trayectorias", height = "480px")
        )
      ),
      
      nav_panel(
        title = "2. Recursión vs. Simulación",
        card_body(
          p(HTML("Validación de la probabilidad exacta \\(\\psi(u)\\) calculada con la fórmula recursiva 
                 (<i>Proposición 7.1</i>: \\(\\psi(u) = \\sum_{y=u}^{\\infty} \\bar{F}(y) + \\sum_{y=0}^{u-1} \\psi(u-y)\\bar{F}(y)\\)) 
                 frente a la proporción empírica de ruina en Monte Carlo.")),
          plotOutput("plot_validacion", height = "480px")
        )
      ),
      
      nav_panel(
        title = "3. Ley Grandes Números",
        card_body(
          p(HTML("Convergencia casi segura \\(\\frac{C_n}{n} \\to 1 - E[Y]\\) (<i>Proposición 7.2</i>). 
                 Bajo \\(E[Y] < 1\\) la deriva es estrictamente positiva, lo que garantiza \\(\\lim_{u \\to \\infty} \\psi(u) = 0\\).")),
          plotOutput("plot_lln", height = "480px")
        )
      ),
      
      nav_panel(
        title = "4. Calculadora Recursiva",
        card_body(
          layout_columns(
            col_widths = c(5, 7),
            card(
              card_header(HTML("Distribución de Reclamación \\(Y\\)")),
              tableOutput("tabla_dist_Y")
            ),
            card(
              card_header(HTML("Probabilidades de Ruina \\(\\psi(u)\\): Exactas vs. Simuladas")),
              tableOutput("tabla_psi_recursiva")
            )
          )
        )
      ),
      
      nav_panel(
        title = "5. Tiempo (τ) y Severidad",
        card_body(
          p(HTML("Distribución condicional del tiempo de ruina (\\(\\tau \\mid \\tau \\le N\\)) y de la 
                 <b>severidad de la ruina</b> (déficit \\(-C_\\tau \\ge 0\\) en el instante de quiebra).")),
          uiOutput("ui_ruina_severidad")
        )
      )
    )
  )
)

# ==============================================================================
# 3. LÓGICA DEL SERVIDOR (SERVER)
# ==============================================================================

server <- function(input, output, session) {
  
  observeEvent(input$reset_ej1, {
    updateNumericInput(session, "f0", value = 0.5)
    updateNumericInput(session, "f1", value = 0.2)
    updateNumericInput(session, "f2", value = 0.3)
    updateNumericInput(session, "f3", value = 0.0)
  })
  
  output$aviso_suma_probs <- renderUI({
    suma <- sum(c(input$f0, input$f1, input$f2, input$f3), na.rm = TRUE)
    if (abs(suma - 1) > 1e-4) {
      div(
        class = "alert alert-warning p-2 small mb-2",
        sprintf("Nota: suman %.3f; se normalizarán a 1.", suma)
      )
    }
  })
  
  params_react <- reactive({
    list(
      f0 = input$f0, f1 = input$f1, f2 = input$f2, f3 = input$f3,
      p_jugador = input$p_jugador,
      p_geom = input$p_geom
    )
  })
  
  dist_react <- reactive({
    obtener_distribucion(input$modelo, params_react())
  })
  
  psi_teorico_react <- reactive({
    u_lim <- max(input$u_max_tabla, input$u_inicial)
    calcular_psi_recursivo(dist_react(), u_lim, input$modelo, params_react())
  })
  
  # ----------------------------------------------------------------------------
  # MOTOR DE ANIMACIÓN SINCRONIZADO (SIN SATURACIÓN DE R)
  # ----------------------------------------------------------------------------
  traj_unica <- reactiveVal(NULL)
  paso_actual <- reactiveVal(0)
  animando <- reactiveVal(FALSE)
  pasos_programaticos_en_vuelo <- reactiveVal(0L)
  
  fijar_paso <- function(nuevo_val, max_val = NULL) {
    paso_actual(nuevo_val)
    val_actual_cliente <- isolate(input$slider_paso)
    if (!is.null(val_actual_cliente) && val_actual_cliente != nuevo_val) {
      pasos_programaticos_en_vuelo(isolate(pasos_programaticos_en_vuelo()) + 1L)
    }
    if (is.null(max_val)) {
      updateSliderInput(session, "slider_paso", value = nuevo_val)
    } else {
      updateSliderInput(session, "slider_paso", value = nuevo_val, max = max_val)
    }
  }
  
  detener_animacion <- function() {
    if (isolate(animando())) {
      animando(FALSE)
    }
    session$sendCustomMessage("control_anim", list(playing = FALSE))
  }
  
  # Regenerar trayectoria cuando cambian parámetros del modelo
  observe({
    dist <- dist_react()
    u0 <- input$u_inicial
    N <- input$horizonte_N
    mod <- input$modelo
    pars <- params_react()
    
    isolate({
      detener_animacion()
      traj_unica(generar_trayectoria_individual(u0, N, mod, pars, dist, forzar_ruina = FALSE))
      fijar_paso(0, max_val = N)
    })
  })
  
  # Detener automáticamente si cambias a la pestaña de Laboratorio
  observeEvent(input$nav_principal, {
    if (input$nav_principal != "intro") {
      detener_animacion()
    }
  })
  
  observeEvent(input$btn_play_pause, {
    traj <- traj_unica()
    req(traj)
    if (!animando()) {
      limite <- if (!is.na(traj$tau)) traj$tau else traj$N
      if (paso_actual() >= limite) {
        fijar_paso(0)
      }
      animando(TRUE)
      session$sendCustomMessage("control_anim", list(
        playing = TRUE,
        delay = input$vel_anim * 1000,
        step_now = TRUE
      ))
    } else {
      detener_animacion()
    }
  })
  
  # Recibe cada tick enviado por JS después de terminar de pintar el frame anterior
  observeEvent(input$anim_tick, {
    req(animando())
    req(input$nav_principal == "intro")
    traj <- traj_unica()
    req(traj)
    
    limite <- if (!is.na(traj$tau)) traj$tau else traj$N
    if (paso_actual() < limite) {
      nuevo <- paso_actual() + 1
      fijar_paso(nuevo)
      if (nuevo >= limite) {
        detener_animacion()
      }
    } else {
      detener_animacion()
    }
  })
  
  observeEvent(input$btn_step, {
    detener_animacion()
    traj <- traj_unica()
    req(traj)
    if (paso_actual() < traj$N) {
      fijar_paso(paso_actual() + 1)
    }
  })
  
  observeEvent(input$btn_reset_paso, {
    detener_animacion()
    fijar_paso(0)
  })
  
  observeEvent(input$btn_nueva_traj, {
    detener_animacion()
    traj_unica(generar_trayectoria_individual(
      input$u_inicial, input$horizonte_N, input$modelo, params_react(), dist_react(), forzar_ruina = FALSE
    ))
    fijar_paso(0)
  })
  
  observeEvent(input$btn_forzar_ruina, {
    detener_animacion()
    traj_unica(generar_trayectoria_individual(
      input$u_inicial, input$horizonte_N, input$modelo, params_react(), dist_react(), forzar_ruina = TRUE
    ))
    fijar_paso(0)
  })
  
  observeEvent(input$slider_paso, {
    req(!is.null(input$slider_paso))
    en_vuelo <- pasos_programaticos_en_vuelo()
    if (en_vuelo > 0L) {
      pasos_programaticos_en_vuelo(en_vuelo - 1L)
    } else {
      detener_animacion()
      paso_actual(input$slider_paso)
    }
  }, ignoreInit = TRUE)
  
  output$telemetria_trayectoria <- renderUI({
    traj <- traj_unica()
    req(traj)
    k <- min(paso_actual(), traj$N)
    
    y_actual_txt <- if (k == 0) "\u2014" else as.character(traj$Y[k])
    y_acum_val <- traj$Y_acum[k + 1]
    c_val <- traj$C[k + 1]
    en_ruina <- (k >= 1) && (c_val <= 0)
    
    layout_columns(
      col_widths = c(2, 2, 2, 3, 3),
      class = "mb-2",
      div(
        class = "telemetry-card",
        div(class = "telemetry-label", HTML("Periodo actual (<i>n</i>)")),
        div(class = "telemetry-val", sprintf("%d / %d", k, traj$N))
      ),
      div(
        class = "telemetry-card",
        div(class = "telemetry-label", HTML("Capital + Primas (<i>u + n</i>)")),
        div(class = "telemetry-val", sprintf("%d + %d = %d", traj$u0, k, traj$u0 + k))
      ),
      div(
        class = "telemetry-card",
        div(class = "telemetry-label", HTML("Reclamación (<i>Y<sub>n</sub></i>)")),
        div(class = "telemetry-val", y_actual_txt)
      ),
      div(
        class = "telemetry-card",
        div(class = "telemetry-label", HTML("Reclam. acum. (&Sigma;<i>Y<sub>j</sub></i>)")),
        div(class = "telemetry-val", as.character(y_acum_val))
      ),
      div(
        class = paste("telemetry-card", if (en_ruina) "telemetry-ruina" else ""),
        div(class = "telemetry-label", HTML("Capital actual (<i>C<sub>n</sub></i>)")),
        div(class = "telemetry-val", sprintf("%d %s", c_val, if (en_ruina) "(RUINA)" else ""))
      )
    )
  })
  
  output$estado_paso_actual <- renderUI({
    traj <- traj_unica()
    req(traj)
    k <- min(paso_actual(), traj$N)
    
    if (k == 0) {
      return(div(
        class = "alert alert-secondary py-2 px-3 narrative-box",
        HTML(sprintf(
          "<span><b>Inicio (n = 0):</b> La aseguradora parte con capital inicial <b>C<sub>0</sub> = u = %d</b>. Presiona <b>Reproducir</b> o <b>Paso +1</b> para iniciar.</span>",
          traj$u0
        ))
      ))
    }
    
    c_prev <- traj$C[k]
    y_curr <- traj$Y[k]
    c_curr <- traj$C[k + 1]
    en_ruina <- c_curr <= 0
    
    if (en_ruina) {
      div(
        class = "alert alert-danger py-2 px-3 narrative-box",
        HTML(sprintf(
          "<span><b>¡RUINA EN n = %d! (&tau; = %d):</b> C<sub>%d</sub> = %d + 1 (prima) &minus; %d (reclamación Y<sub>%d</sub>) &rArr; <b>C<sub>%d</sub> = %d &le; 0</b>. Severidad: <b>%d</b>.</span>",
          k, k, k - 1, c_prev, y_curr, k, k, c_curr, -c_curr
        ))
      )
    } else {
      div(
        class = "alert alert-info py-2 px-3 narrative-box",
        HTML(sprintf(
          "<span><b>Periodo n = %d:</b> C<sub>%d</sub> = %d + 1 (prima) &minus; %d (reclamación Y<sub>%d</sub>) &rArr; <b>C<sub>%d</sub> = %d &gt; 0</b> (la compañía continúa).</span>",
          k, k - 1, c_prev, y_curr, k, k, c_curr
        ))
      )
    }
  })
  
  output$plot_animacion_unica <- renderPlot({
    traj <- traj_unica()
    req(traj)
    k <- min(paso_actual(), traj$N)
    
    y_min <- min(-2, min(traj$C) - 1)
    y_max <- max(traj$u0 + 4, max(traj$C + 1) + 1.5)
    
    p <- ggplot() +
      annotate("rect", xmin = -0.5, xmax = traj$N + 2.5, ymin = y_min - 2, ymax = 0,
               fill = "#FADBD8", alpha = 0.45) +
      geom_hline(yintercept = 0, color = "#C0392B", linewidth = 1) +
      annotate("text", x = traj$N + 2.0, y = -0.45,
               label = 'paste("Umbral de ruina (", C[n] <= 0, ")")', parse = TRUE,
               hjust = 1, color = "#922B21", fontface = "bold", size = 3.8)
    
    if (k >= 1) {
      df_seg_prima <- data.frame(
        x = 0:(k - 1),
        xend = 1:k,
        y = traj$C[1:k],
        yend = traj$C[1:k] + 1
      )
      df_seg_reclamo <- data.frame(
        x = 1:k,
        xend = 1:k,
        y = traj$C[1:k] + 1,
        yend = traj$C[2:(k + 1)]
      )
      df_abiertos <- data.frame(
        x = 1:k,
        y = traj$C[1:k] + 1
      )
      
      p <- p +
        geom_segment(data = df_seg_prima, aes(x = x, y = y, xend = xend, yend = yend),
                     linetype = "dotted", color = "#1F618D", linewidth = 1) +
        geom_segment(data = df_seg_reclamo, aes(x = x, y = y, xend = xend, yend = yend),
                     linetype = "dashed", color = "#7F8C8D", linewidth = 0.9) +
        geom_point(data = df_abiertos, aes(x = x, y = y),
                   shape = 21, fill = "white", color = "#1F618D", size = 3.2, stroke = 1.1) +
        annotate(
          "text",
          x = k + 0.3,
          y = (traj$C[k] + 1 + traj$C[k + 1]) / 2,
          label = sprintf("-Y[%d] == -%d", k, traj$Y[k]),
          parse = TRUE,
          hjust = 0,
          color = "#566573",
          size = 3.7
        )
    }
    
    n_seq <- 0:k
    c_seq <- traj$C[1:(k + 1)]
    es_ruina <- (n_seq >= 1) & (c_seq <= 0)
    
    df_no_ruina <- data.frame(n = n_seq[!es_ruina], C = c_seq[!es_ruina])
    df_si_ruina <- data.frame(n = n_seq[es_ruina], C = c_seq[es_ruina])
    
    if (nrow(df_no_ruina) > 0) {
      p <- p + geom_point(data = df_no_ruina, aes(x = n, y = C), color = "#1B4F72", size = 3.8)
    }
    if (nrow(df_si_ruina) > 0) {
      p <- p + geom_point(data = df_si_ruina, aes(x = n, y = C), color = "#C0392B", size = 5.2)
    }
    
    if (!is.na(traj$tau) && k >= traj$tau) {
      c_tau <- traj$C[traj$tau + 1]
      p <- p +
        annotate("label", x = traj$tau, y = c_tau,
                 label = sprintf("\u03c4 = %d (C_\u03c4 = %d)", traj$tau, c_tau),
                 vjust = 1.45, fill = "#C0392B", color = "white", fontface = "bold", size = 3.8)
    }
    
    p +
      scale_y_continuous(breaks = pretty_breaks(n = 6)) +
      coord_cartesian(xlim = c(0, traj$N + 2.2), ylim = c(y_min, y_max), expand = FALSE) +
      labs(
        title = bquote(paste("Evolución individual del capital ", C[n], "  |  ", u == .(traj$u0), ",  ", n == .(k), " de ", .(traj$N))),
        subtitle = expression(paste("Línea punteada azul: prima (+1)   |   Línea vertical gris: reclamación (", -Y[n], ")")),
        x = expression(paste("Periodo (", n, ")")),
        y = expression(paste("Capital de la aseguradora ", C[n]))
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        plot.margin = margin(12, 20, 12, 12)
      )
  })
  
  # ----------------------------------------------------------------------------
  # SIMULACIONES MONTE CARLO MASIVAS (VISTA 2: LABORATORIO)
  # ----------------------------------------------------------------------------
  sim_react <- reactive({
    input$resimular
    dist <- dist_react()
    n_sim <- input$n_sim
    N <- input$horizonte_N
    
    Y_mat <- matrix(
      simular_Y(n_sim * N, input$modelo, params_react(), dist),
      nrow = n_sim, ncol = N
    )
    
    Z_mat <- 1 - Y_mat
    cum_Z <- t(apply(Z_mat, 1, cumsum))
    min_cum_Z <- apply(cum_Z, 1, min)
    
    u0 <- input$u_inicial
    C_mat <- cbind(rep(u0, n_sim), u0 + cum_Z)
    
    ruina_por_sim <- min_cum_Z <= -u0
    tau_vec <- rep(NA_integer_, n_sim)
    severidad_vec <- rep(NA_real_, n_sim)
    
    idx_ruina <- which(ruina_por_sim)
    if (length(idx_ruina) > 0) {
      for (i in idx_ruina) {
        primer_col <- which(C_mat[i, -1] <= 0)[1]
        tau_vec[i] <- primer_col
        severidad_vec[i] <- -C_mat[i, primer_col + 1]
      }
    }
    
    list(
      C_mat = C_mat,
      min_cum_Z = min_cum_Z,
      ruina_por_sim = ruina_por_sim,
      tau_vec = tau_vec,
      severidad_vec = severidad_vec
    )
  })
  
  # ----------------------------------------------------------------------------
  # KPIs SUPERIORES (VISTA 2: SEMÁFORO COMPACTO)
  # ----------------------------------------------------------------------------
  output$kpi_boxes <- renderUI({
    dist <- dist_react()
    EY <- dist$EY
    cumple_gn <- EY < 1 - 1e-12
    
    psi_df <- psi_teorico_react()
    psi_u <- psi_df$psi_recursivo[psi_df$u == input$u_inicial]
    
    sim <- sim_react()
    psi_emp <- mean(sim$ruina_por_sim)
    
    layout_columns(
      fill = FALSE,
      value_box(
        title = HTML("Ganancia Neta (E[Y] &lt; 1)"),
        value = sprintf("E[Y] = %.4f", EY),
        p(if (cumple_gn) "CUMPLE (Deriva > 0)" else "NO CUMPLE (Ruina segura)", class = "small mb-0"),
        showcase = icon(if (cumple_gn) "circle-check" else "triangle-exclamation"),
        showcase_layout = "left center",
        theme = if (cumple_gn) "success" else "danger"
      ),
      value_box(
        title = HTML(sprintf("Ruina Teórica &psi;(%d)", input$u_inicial)),
        value = sprintf("%.4f (%.2f%%)", psi_u, 100 * psi_u),
        p(HTML(sprintf("Con capital cero: &psi;(0) = %.4f", psi_df$psi_recursivo[1])), class = "small mb-0"),
        showcase = icon("calculator"),
        showcase_layout = "left center",
        theme = "primary"
      ),
      value_box(
        title = HTML(sprintf("Ruina Simulada &psi;&#770;(%d)", input$u_inicial)),
        value = sprintf("%.4f (%.2f%%)", psi_emp, 100 * psi_emp),
        p(sprintf("%s carteras (N = %d)", comma(input$n_sim), input$horizonte_N), class = "small mb-0"),
        showcase = icon("chart-line"),
        showcase_layout = "left center",
        theme = "info"
      )
    )
  })
  
  # ----------------------------------------------------------------------------
  # GRÁFICA 1: MÚLTIPLES TRAYECTORIAS
  # ----------------------------------------------------------------------------
  output$plot_trayectorias <- renderPlot({
    sim <- sim_react()
    dist <- dist_react()
    
    m <- min(input$n_trayectorias, nrow(sim$C_mat))
    N <- input$horizonte_N
    u0 <- input$u_inicial
    
    df_list <- lapply(1:m, function(i) {
      tau_i <- sim$tau_vec[i]
      arruinada <- !is.na(tau_i)
      data.frame(
        id = factor(i),
        n = 0:N,
        C = sim$C_mat[i, ],
        estado = ifelse(arruinada, "Con ruina (\u03c4 < \u221e)", "Sin ruina en [1, N]"),
        es_tau = (0:N) == ifelse(arruinada, tau_i, -1)
      )
    })
    df_traj <- bind_rows(df_list)
    df_puntos_tau <- df_traj[df_traj$es_tau, ]
    
    df_esperanza <- data.frame(
      n = 0:N,
      E_Cn = u0 + (0:N) * (1 - dist$EY)
    )
    
    ggplot(df_traj, aes(x = n, y = C, group = id, color = estado)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.8) +
      geom_line(alpha = 0.75, linewidth = 0.8) +
      geom_point(data = df_puntos_tau, size = 3, shape = 19) +
      geom_line(
        data = df_esperanza,
        aes(x = n, y = E_Cn, group = 1),
        inherit.aes = FALSE,
        color = "#111111", linetype = "dashed", linewidth = 1.1
      ) +
      scale_color_manual(
        values = c("Con ruina (\u03c4 < \u221e)" = "#C0392B", "Sin ruina en [1, N]" = "#2980B9")
      ) +
      labs(
        title = bquote(paste("Simulación de ", .(m), " trayectorias del proceso de riesgo (", u == .(u0), ")")),
        subtitle = bquote(paste("Línea punteada negra: Esperanza teórica ", E[C[n]] == .(u0) + n * (1 - .(round(dist$EY, 3))))),
        x = expression(paste("Tiempo / Periodo (", n, ")")),
        y = expression(paste("Capital de la aseguradora ", C[n])),
        color = "Trayectoria"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", plot.title = element_text(face = "bold"))
  })
  
  # ----------------------------------------------------------------------------
  # GRÁFICA 2: VALIDACIÓN RECURSIVA VS MONTE CARLO
  # ----------------------------------------------------------------------------
  output$plot_validacion <- renderPlot({
    psi_df <- psi_teorico_react()
    psi_df <- psi_df[psi_df$u <= input$u_max_tabla, ]
    sim <- sim_react()
    n_sim <- input$n_sim
    
    psi_df$psi_sim <- sapply(psi_df$u, function(k) mean(sim$min_cum_Z <= -k))
    psi_df$se <- sqrt(psi_df$psi_sim * (1 - psi_df$psi_sim) / n_sim)
    
    ggplot(psi_df, aes(x = factor(u))) +
      geom_col(aes(y = psi_recursivo, fill = "Teórico (Fórmula Recursiva)"), alpha = 0.75, width = 0.65) +
      geom_errorbar(
        aes(ymin = pmax(0, psi_sim - 1.96 * se), ymax = pmin(1, psi_sim + 1.96 * se)),
        width = 0.2, color = "#111111", linewidth = 0.7
      ) +
      geom_point(aes(y = psi_sim, color = "Simulado (Monte Carlo \u00b1 IC 95%)"), size = 3.2) +
      scale_fill_manual(values = c("Teórico (Fórmula Recursiva)" = "#2E86C1")) +
      scale_color_manual(values = c("Simulado (Monte Carlo \u00b1 IC 95%)" = "#C0392B")) +
      scale_y_continuous(limits = c(0, 1.05), labels = percent_format(accuracy = 1)) +
      labs(
        title = expression(paste("Probabilidad de ruina ", psi(u), ": Fórmula Recursiva vs. Monte Carlo")),
        subtitle = bquote(paste("Comparación para ", u == 0, ", ..., ", .(input$u_max_tabla), "  (", N == .(input$horizonte_N), " periodos)")),
        x = expression(paste("Capital inicial (", u, ")")),
        y = expression(paste("Probabilidad de ruina ", psi(u))),
        fill = NULL,
        color = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", plot.title = element_text(face = "bold"))
  })
  
  # ----------------------------------------------------------------------------
  # GRÁFICA 3: LEY FUERTE DE LOS GRANDES NÚMEROS
  # ----------------------------------------------------------------------------
  output$plot_lln <- renderPlot({
    dist <- dist_react()
    u0 <- input$u_inicial
    N_lln <- max(200, input$horizonte_N * 3)
    m_lln <- 12
    
    Y_lln <- matrix(
      simular_Y(m_lln * N_lln, input$modelo, params_react(), dist),
      nrow = m_lln, ncol = N_lln
    )
    C_lln <- u0 + t(apply(1 - Y_lln, 1, cumsum))
    n_seq <- 1:N_lln
    
    df_lln <- bind_rows(lapply(1:m_lln, function(i) {
      data.frame(id = factor(i), n = n_seq, promedio = C_lln[i, ] / n_seq)
    }))
    
    limite_teorico <- 1 - dist$EY
    
    ggplot(df_lln, aes(x = n, y = promedio, group = id)) +
      geom_hline(yintercept = 0, color = "gray40", linetype = "dotted", linewidth = 0.8) +
      geom_line(color = "#2874A6", alpha = 0.55, linewidth = 0.7) +
      geom_hline(yintercept = limite_teorico, color = "#C0392B", linetype = "dashed", linewidth = 1.1) +
      annotate(
        "label",
        x = N_lln * 0.76,
        y = limite_teorico,
        label = sprintf("Límite c.s.: 1 - E[Y] = %.3f", limite_teorico),
        color = "#C0392B",
        fontface = "bold",
        vjust = -0.5
      ) +
      labs(
        title = expression(paste("Ley Fuerte de los Grandes Números: Convergencia de ", C[n] / n)),
        subtitle = expression(paste("Cuando ", E[Y] < 1, ",  ", C[n] %->% +infinity, " c.s.  y  ", lim[u %->% infinity] ~ psi(u) == 0)),
        x = expression(paste("Número de periodos (", n, ")")),
        y = expression(paste("Promedio temporal ", C[n] / n))
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))
  })
  
  # ----------------------------------------------------------------------------
  # PESTAÑA 4: TABLAS DE LA CALCULADORA RECURSIVA
  # ----------------------------------------------------------------------------
  output$tabla_dist_Y <- renderTable({
    dist <- dist_react()
    data.frame(
      y = as.integer(dist$y_tabla),
      "f(y) = P(Y = y)" = round(dist$f_tabla, 4),
      "F(y) = P(Y <= y)" = round(dist$F_tabla, 4),
      "Cola 1 - F(y)" = round(dist$F_bar_tabla, 4),
      check.names = FALSE
    )
  }, digits = 4)
  
  output$tabla_psi_recursiva <- renderTable({
    psi_df <- psi_teorico_react()
    psi_df <- psi_df[psi_df$u <= input$u_max_tabla, ]
    sim <- sim_react()
    psi_sim <- sapply(psi_df$u, function(k) mean(sim$min_cum_Z <= -k))
    
    data.frame(
      u = as.integer(psi_df$u),
      "\u03c8(u) Recursivo" = round(psi_df$psi_recursivo, 6),
      "\u03c8(u) F\u00f3rm. Cerrada" = ifelse(is.na(psi_df$psi_cerrada), "\u2014", sprintf("%.6f", psi_df$psi_cerrada)),
      "\u03c8\u0302(u) Simulado" = round(psi_sim, 4),
      "Error Abs." = round(abs(psi_df$psi_recursivo - psi_sim), 4),
      check.names = FALSE
    )
  })
  
  # ----------------------------------------------------------------------------
  # PESTAÑA 5: TIEMPO DE RUINA Y SEVERIDAD
  # ----------------------------------------------------------------------------
  output$ui_ruina_severidad <- renderUI({
    sim <- sim_react()
    n_ruinas <- sum(sim$ruina_por_sim)
    
    if (n_ruinas < 5) {
      return(
        div(
          class = "alert alert-info mt-3",
          h5("Muy pocas trayectorias llegaron a la ruina con estos parámetros"),
          p(HTML(sprintf(
            "Solo %d de %d carteras quebraron (con \\(u = %d\\) y \\(E[Y] = %.3f\\)). Reduce el capital inicial \\(u\\) o acerca \\(E[Y]\\) a 1 para ver los histogramas.",
            n_ruinas, input$n_sim, input$u_inicial, dist_react()$EY
          )))
        )
      )
    }
    
    layout_columns(
      col_widths = c(6, 6),
      plotOutput("plot_tau", height = "420px"),
      plotOutput("plot_severidad", height = "420px")
    )
  })
  
  output$plot_tau <- renderPlot({
    sim <- sim_react()
    taus <- sim$tau_vec[!is.na(sim$tau_vec)]
    req(length(taus) >= 5)
    
    ggplot(data.frame(tau = taus), aes(x = tau)) +
      geom_histogram(binwidth = max(1, round(input$horizonte_N / 25)), fill = "#D35400", color = "white", alpha = 0.85) +
      geom_vline(xintercept = mean(taus), linetype = "dashed", color = "#111111", linewidth = 0.9) +
      labs(
        title = expression(paste("Distribución condicional del tiempo de ruina (", tau ~ "|" ~ tau <= N, ")")),
        subtitle = bquote(paste("Promedio ", E[tau ~ "|" ~ tau <= N] == .(round(mean(taus), 2)), " periodos (", .(length(taus)), " ruinas)")),
        x = expression(paste("Tiempo de ruina (", tau, ")")),
        y = "Frecuencia"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold"))
  })
  
  output$plot_severidad <- renderPlot({
    sim <- sim_react()
    sevs <- sim$severidad_vec[!is.na(sim$severidad_vec)]
    req(length(sevs) >= 5)
    
    ggplot(data.frame(severidad = factor(sevs)), aes(x = severidad)) +
      geom_bar(fill = "#8E44AD", alpha = 0.85, width = 0.6) +
      labs(
        title = expression(paste("Severidad de la ruina (", -C[tau] ~ "|" ~ tau <= N, ")")),
        subtitle = bquote(paste("Déficit promedio al quebrar: ", .(round(mean(sevs), 3)), " unidades monetarias")),
        x = expression(paste("Déficit en el instante de ruina (", -C[tau], ")")),
        y = "Frecuencia"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold"))
  })
}

# ==============================================================================
# 4. LANZAR APLICACIÓN
# ==============================================================================
shinyApp(ui = ui, server = server)

