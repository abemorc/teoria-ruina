# Teoría de la Ruina en Tiempo Discreto — Aplicación Interactiva en R / Shiny

![R](https://img.shields.io/badge/R-%E2%89%A5%204.1.0-276DC3?logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-Web%20App-1B4F72?logo=rstudio&logoColor=white)
![bslib](https://img.shields.io/badge/UI-bslib%20%2F%20Bootstrap%205-5D6D7E)
![License](https://img.shields.io/badge/Licencia-Uso%20Acad%C3%A9mico%20%2F%20MIT-success)

Aplicación web interactiva desarrollada en **R** y **Shiny** para visualizar, simular y validar los resultados fundamentales de la **Teoría de la Ruina en Tiempo Discreto** en ciencias actuariales y procesos estocásticos.

Está diseñada con una arquitectura pedagógica pensada tanto para **exposiciones en clase** (construcción intuitiva paso a paso antes de las demostraciones) como para **laboratorio computacional** (contraste entre fórmulas recursivas exactas y simulaciones Monte Carlo vectorizadas).

---

## Características Principales

### 1. Arquitectura Pedagógica en Tres Vistas
* **Vista 1 — Introducción al Problema (Animación paso a paso):**
  * Muestra una trayectoria individual $C_n$ evolucionando periodo por periodo ($n = 0, 1, \dots, N$).
  * Incluye **5 tarjetas de telemetría en vivo** ($n$, $u+n$, $Y_n$, $\sum Y_j$ y $C_n$), narración aritmética de cada transición y descomposición visual entre el cobro de prima ($+1$) y el pago de siniestros ($-Y_n$).
  * Incorpora reproducción automática sincronizada (de $0.5\text{ s}$ a $10\text{ s}$ por paso) con pausa instantánea y muestreo por rechazo (**Ruta con ruina**) para observar escenarios de quiebra sin alterar los parámetros del modelo.
* **Vista 2 — Laboratorio y Validación Teórica:**
  * **Diagnóstico rápido (KPIs):** Verifica automáticamente la condición de ganancia neta ($E[Y] < 1$) y compara $\psi(u)$ exacto frente al estimador empírico $\hat{\psi}(u)$.
  * **Múltiples Trayectorias:** Visualiza realizaciones simultáneas frente a la deriva determinista esperada $E[C_n] = u + n(1 - E[Y])$.
  * **Recursión vs. Simulación:** Contrasta la probabilidad exacta $\psi(u)$ calculada recursivamente frente a las proporciones simuladas con intervalos de confianza del $95\%$.
  * **Ley Fuerte de los Grandes Números:** Ilustra la convergencia casi segura $\frac{C_n}{n} \to 1 - E[Y]$.
  * **Calculadora Recursiva:** Tablas de alta precisión con $f(y)$, $F(y)$, cola $\bar{F}(y)$, $\psi(u)$ recursivo, fórmula cerrada (cuando existe) y error absoluto frente a Monte Carlo.
  * **Tiempo ($\tau$) y Severidad ($-C_\tau$):** Histogramas condicionales al evento de ruina $\{\tau \le N\}$.
* **Vista 3 — Guía de Usuario Integrada:**
  * Renderiza directamente dentro de la interfaz el manual completo (`guia_usuario.md`) con soporte matemático en MathJax, accesible en cualquier momento sin reiniciar la simulación activa.

### 2. Catálogo de Distribuciones para las Reclamaciones ($Y_i$)
Soporta **8 modelos discretos** sobre $\mathbb{N}_0 = \{0, 1, 2, \dots\}$:
1. **Ejemplo 7.1 (Discreta Tabulada):** Soporte $\{0, 1, 2, 3\}$ con probabilidades editables y normalización automática.
2. **Ejemplo 7.2 (Ruina del Jugador):** $Y \in \{0, 2\}$ con $P(Y=0) = p$.
3. **Ejemplo 7.3 (Geométrica):** $f(y) = (1-p)^y p$ para $y \ge 0$.
4. **Bernoulli($p$):** $Y \in \{0, 1\}$ con $E[Y] = p$.
5. **Binomial($m, p$):** $Y \in \{0, 1, \dots, m\}$ con $E[Y] = mp$.
6. **Poisson($\lambda$):** Reclamaciones con tasa media $E[Y] = \lambda$.
7. **Binomial Negativa($r, p$):** Modela sobredispersión con $E[Y] = \frac{r(1-p)}{p}$.
8. **Distribución Personalizada Dinámica:** Permite elegir entre $k = 2$ y $k = 10$ valores enteros arbitrarios $y_i \ge 0$ con sus respectivas probabilidades o pesos relativos.

---

## Resumen del Modelo Matemático

El superávit (capital) de la aseguradora al final del periodo $n$ se define como:

$$C_n = u + n - \sum_{j=1}^n Y_j, \qquad n = 0, 1, 2, \dots$$

donde $u \ge 0$ es el capital inicial ($C_0 = u$), $+1$ es la prima unitaria determinista por periodo y $\{Y_j\}_{j \ge 1}$ son reclamaciones i.i.d. con función de probabilidad $f(y) = P(Y = y)$.

El **tiempo de ruina** $\tau$ y la **probabilidad de ruina en horizonte infinito** $\psi(u)$ están dados por:

$$\tau = \min\{n \ge 1 : C_n \le 0\}, \qquad \psi(u) = P(\tau < \infty \mid C_0 = u).$$

Bajo la condición de ganancia neta $E[Y] < 1$ y $f(0) > 0$, $\psi(u)$ satisface la relación recursiva exacta:

$$\psi(0) = E[Y], \qquad \psi(u) = \frac{1}{f(0)}\left[\sum_{y=u}^{\infty} \bar{F}(y) + \sum_{y=1}^{u-1} \psi(u-y)\bar{F}(y)\right], \quad u \ge 1,$$

donde $\bar{F}(y) = P(Y > y) = 1 - F(y)$.

---

## Estructura del Repositorio

```text
teoria-ruina/
├── app.R              # Código principal de la aplicación Shiny (motor matemático, UI y servidor)
├── guia_usuario.md    # Documentación detallada renderizada en la pestaña "3. Guía de Usuario"
└── README.md          # Descripción general e instrucciones de instalación del repositorio
```

---

## Requisitos e Instalación

### 1. Requisitos previos
* **R** (versión `4.1.0` o superior recomendada).
* **RStudio** (opcional, recomendado para ejecutar con un clic).

### 2. Instalación de paquetes necesarios
Abre una consola de R y ejecuta el siguiente comando para instalar las dependencias:

```r
install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "scales"))
```

---

## Cómo Ejecutar la Aplicación

1. Clona este repositorio en tu equipo local:
   ```bash
   git clone https://github.com/tu-usuario/teoria-ruina.git
   cd teoria-ruina
   ```
2. Asegúrate de que `app.R` y `guia_usuario.md` se encuentren en el mismo directorio de trabajo.
3. Inicia la aplicación desde R o RStudio:
   ```r
   shiny::runApp()
   ```
   *(También puedes abrir `app.R` en RStudio y presionar el botón **Run App**).*

---

## Licencia

Este proyecto se distribuye con fines educativos y académicos bajo la licencia **MIT**. 


