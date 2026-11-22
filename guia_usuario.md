# Guía de Usuario: Teoría de la Ruina en Tiempo Discreto

Esta guía describe el funcionamiento completo de la aplicación interactiva para el estudio del **proceso de riesgo en tiempo discreto**, detallando su estructura pedagógica, el catálogo de distribuciones de reclamación disponibles, los rangos y valores iniciales (**por defecto**) de todos los controles, y una colección de ejemplos prácticos listos para ejecutar.

---

## 1. Fundamento Matemático y Estructura de la Aplicación

El modelo describe la evolución del superávit (capital) $C\_n$ de una compañía aseguradora al cierre de cada periodo $n \in \\{0, 1, 2, \dots\\}$:

$$C\_n = u + n - \sum\_{j=1}^n Y\_j, \qquad n = 0, 1, 2, \dots$$

donde:
* $u \ge 0$ es el **capital inicial** ($C\_0 = u$).
* $+1$ es la **prima unitaria determinista** cobrada en cada periodo.
* $Y\_1, Y\_2, \dots$ son variables aleatorias independientes e idénticamente distribuidas (i.i.d.) con soporte en los enteros no negativos $\\{0, 1, 2, \dots\\}$ y función de probabilidad puntual $f(y) = P(Y = y)$, que representan el **monto total de reclamaciones** en cada periodo.

El **tiempo de ruina** $\tau$ y la **probabilidad de ruina en horizonte infinito** $\psi(u)$ se definen como:

$$\tau = \min\\{n \ge 1 : C\_n \le 0\\}, \qquad \psi(u) = P(\tau < \infty \mid C\_0 = u).$$

### Navegación Superior en Tres Vistas
La barra superior divide la aplicación en **tres vistas independientes** para acompañar el ritmo de una exposición o sesión de laboratorio sin perder los parámetros configurados:

1. **1. Introducción al Problema:** Diseñada para abrir la explicación antes de deducir fórmulas. Muestra una única trayectoria individual avanzando paso a paso con tarjetas de telemetría en vivo, ocultando las simulaciones masivas para evitar distracciones.
2. **2. Laboratorio y Validación Teórica:** Diseñada para contrastar los teoremas del pizarrón con resultados exactos y simulaciones Monte Carlo mediante indicadores tipo semáforo y cinco pestañas analíticas.
3. **3. Guía de Usuario:** Documentación de referencia integrada y accesible desde cualquier punto de la aplicación sin reiniciar ni alterar la simulación en curso.

---

## 2. Panel Lateral: Parámetros del Proceso

El panel izquierdo controla las variables estructurales del modelo. Los primeros tres bloques están disponibles en todo momento, mientras que el cuarto bloque se despliega automáticamente al entrar a la **Vista 2**.

### Bloque 1: Distribución de reclamaciones ($Y$)

Permite seleccionar entre **8 distribuciones discretas** para los montos de reclamación $Y\_j$:

| Modelo en el selector | Soporte de $Y$ | Parámetros y valores por defecto | Esperanza $E[Y]$ | Condición de ganancia neta ($E[Y] < 1$) |
| :--- | :--- | :--- | :--- | :--- |
| **Ejemplo 7.1: Discreta Tabulada** *(por defecto)* | $\\{0, 1, 2, 3\\}$ | $f(0)=0.50$, $f(1)=0.20$, $f(2)=0.30$, $f(3)=0.00$ | $0.8000$ | $\sum\_{y=0}^3 y\,f(y) < 1$ |
| **Ejemplo 7.2: Ruina del Jugador (0 o 2)** | $\\{0, 2\\}$ | $p = P(Y=0) \in [0.05, 0.95]$ *(defecto: $0.65$)* | $2(1-p) = 0.7000$ | $p > 0.50$ |
| **Ejemplo 7.3: Geométrica** | $\\{0, 1, 2, \dots\\}$ | $p \in [0.05, 0.95]$ *(defecto: $0.60$)* | $\frac{1-p}{p} \approx 0.6667$ | $p > 0.50$ |
| **Bernoulli($p$)** | $\\{0, 1\\}$ | $p = P(Y=1) \in [0.05, 0.95]$ *(defecto: $0.40$)* | $p = 0.4000$ | $p < 1$ (si $u \ge 1$, $\psi(u) = 0$) |
| **Binomial($m, p$)** | $\\{0, 1, \dots, m\\}$ | $m \in [1, 20]$ *(defecto: $4$)*, $p \in [0.01, 0.95]$ *(defecto: $0.20$)* | $m p = 0.8000$ | $m p < 1$ |
| **Poisson($\lambda$)** | $\\{0, 1, 2, \dots\\}$ | $\lambda \in [0.05, 3.00]$ *(defecto: $0.75$)* | $\lambda = 0.7500$ | $\lambda < 1$ |
| **Binomial Negativa($r, p$)** | $\\{0, 1, 2, \dots\\}$ | $r \in [1, 15]$ *(defecto: $2$)*, $p \in [0.10, 0.95]$ *(defecto: $0.75$)* | $\frac{r(1-p)}{p} \approx 0.6667$ | $\frac{r(1-p)}{p} < 1$ |
| **Personalizada (Valores y Prob. libres)** | $k$ valores enteros $\ge 0$ | $k \in [2, 10]$ *(defecto: $k=4$; $y = 0,1,2,4$; $p = 0.55, 0.20, 0.15, 0.10$)* | $\sum\_i y\_i f(y\_i) = 0.9000$ | $\sum\_i y\_i f(y\_i) < 1$ |

#### Detalles especiales de las distribuciones tabuladas y personalizadas:
* **Normalización automática:** Tanto en el *Ejemplo 7.1* como en la *Distribución Personalizada*, si los pesos o probabilidades ingresados no suman exactamente $1$, la aplicación muestra una notificación informativa y los normaliza automáticamente dividiendo entre su suma total.
* **Constructor dinámico de la Distribución Personalizada:** Al elegir esta opción, el campo **Cantidad de valores distintos ($k$)** genera dinámicamente entre $2$ y $10$ filas con dos columnas: **Valor $y$** (monto entero no negativo del reclamo) y **Prob. $f(y)$** (su probabilidad o peso relativo). Si se repiten dos valores de $y$, sus probabilidades se suman automáticamente. Debajo aparece una etiqueta en vivo con el valor resultante de $E[Y]$ y el estado de ganancia neta.

### Bloques 2, 3 y 4: Capital, Horizonte y Monte Carlo

| Control | Ubicación | Rango permitido | Valor por defecto | Descripción |
| :--- | :--- | :--- | :--- | :--- |
| **2. Capital inicial ($u$)** | Siempre visible | $0$ a $15$ (paso $1$) | **$3$** | Fija el superávit inicial $C\_0 = u$. |
| **3. Horizonte de tiempo ($N$)** | Siempre visible | $15$ a $120$ (paso $5$) | **$35$** | Número máximo de periodos simulados en cada trayectoria. |
| **Capital máximo en tablas/gráficas** | Solo Vista 2 | $5$ a $20$ (paso $1$) | **$10$** | Límite superior $u\_{\max}$ para las tablas y barras de $\psi(u)$. |
| **Trayectorias en Pestaña 1** | Solo Vista 2 | $5$ a $35$ (paso $1$) | **$15$** | Cantidad de rutas simultáneas dibujadas en la gráfica de trayectorias. |
| **Carteras simuladas** | Solo Vista 2 | $500$ a $10{,}000$ (paso $500$) | **$3{,}000$** | Tamaño de muestra Monte Carlo ($n\_{\text{sim}}$). |
| **Botón *Nuevas simulaciones*** | Solo Vista 2 | Acción | --- | Genera un nuevo lote independiente de $n\_{\text{sim}}$ carteras. |

---

## 3. Vista 1: Introducción al Problema (Animación Paso a Paso)

Al abrir la aplicación, la visualización comienza **pausada en el periodo $n = 0$** para permitir presentar el estado inicial antes de iniciar la dinámica temporal.

### Controles de Reproducción
* **Reproducir / Pausar:** Inicia o detiene el avance automático periodo por periodo. El reloj está sincronizado con el dibujado de la gráfica, por lo que al presionar **Pausar** la detención es inmediata ($0\text{ ms}$) y el avance se detiene automáticamente si la trayectoria alcanza el tiempo de ruina $\tau$.
* **Paso +1:** Avanza exactamente un periodo ($n \to n+1$) manteniendo la animación en pausa, ideal para explicar cada operación aritmética frente al grupo.
* **Reiniciar ($n=0$):** Regresa al instante inicial $n=0$ conservando exactamente la misma secuencia de reclamaciones $\\{Y\_1, \dots, Y\_N\\}$ para repetir su análisis.
* **Nueva ruta:** Genera una nueva trayectoria aleatoria independiente bajo la distribución configurada.
* **Ruta con ruina:** Genera realizaciones independientes bajo **exactamente la misma distribución de $Y$** (sin alterar $E[Y]$ ni los parámetros), filtrando mediante muestreo por rechazo hasta encontrar una trayectoria donde ocurra $\tau \le N$. Es muy útil cuando $E[Y] < 1$ y la mayoría de las trayectorias sobreviven.
* **Explorar periodo ($n$):** Deslizador de $0$ a $N$ *(por defecto $0$)* para moverse manualmente a cualquier instante de la trayectoria.
* **Tiempo entre pasos (segundos):** Deslizador de $0.5$ a $10.0\text{ s}$ en incrementos de $0.5\text{ s}$ *(por defecto $1.5\text{ s}$)* que ajusta la velocidad en tiempo real incluso mientras la animación está corriendo.

### Telemetría en Vivo y Lectura de la Gráfica
Debajo de los controles se muestran **cinco tarjetas numéricas** sincronizadas con el periodo actual $n$:
1. **Periodo actual ($n$):** Progreso $n / N$.
2. **Capital + Primas ($u + n$):** Recursos acumulados antes de descontar siniestros.
3. **Reclamación ($Y\_n$):** Monto aleatorio ocurrido en el periodo vigente (muestra ``—`` en $n=0$).
4. **Reclamaciones acumuladas ($\sum Y\_j$):** Suma total de siniestros hasta el periodo $n$.
5. **Capital actual ($C\_n$):** Saldo neto vigente. Si $n \ge 1$ y $C\_n \le 0$, la tarjeta se ilumina en rojo con la etiqueta **(RUINA)**.

En la **gráfica principal**, cada periodo $n$ se descompone en dos movimientos visuales:
* **Segmento punteado azul:** Subida determinista de $+1$ por el cobro de la prima desde $C\_{n-1}$ hasta el círculo abierto en $(n, C\_{n-1} + 1)$.
* **Segmento vertical discontinuo gris:** Caída aleatoria $-Y\_n$ desde el círculo abierto hasta el punto sólido azul $(n, C\_n)$.
* **Zona roja inferior ($C\_n \le 0$):** Región de ruina. Al tocar o cruzar el eje $0$, aparece un punto rojo grande con la etiqueta $\tau = n$ y el saldo $C\_\tau$.

---

## 4. Vista 2: Laboratorio y Validación Teórica

Al cambiar a **2. Laboratorio y Validación Teórica**, la parte superior muestra **tres indicadores clave (KPIs)**:
* **Ganancia Neta ($E[Y] < 1$):** Muestra el valor exacto de $E[Y]$. Se ilumina en **verde** (*CUMPLE (Deriva > 0)*) cuando $E[Y] < 1$, o en **rojo** (*NO CUMPLE (Ruina segura)*) cuando $E[Y] \ge 1$.
* **Ruina Teórica $\psi(u)$:** Presenta la probabilidad exacta calculada mediante la ecuación recursiva junto con el valor de referencia con capital nulo $\psi(0) = E[Y]$.
* **Ruina Simulada $\hat{\psi}(u)$:** Muestra la proporción empírica de carteras arruinadas dentro del horizonte $N$ entre las $n\_{\text{sim}}$ simulaciones de Monte Carlo.

Debajo de los indicadores se despliegan **cinco pestañas analíticas**:

### Pestaña 1: Múltiples Trayectorias
Grafica simultáneamente $m$ realizaciones del proceso $C\_n$ *(por defecto $m=15$)* desde $n=0$ hasta $N$:
* Las rutas en **azul** sobreviven todo el horizonte $[1, N]$.
* Las rutas en **rojo** sufren ruina en algún instante $\tau \le N$, destacando con un punto sólido su primer cruce o toque al eje cero.
* La **línea discontinua negra** traza la trayectoria esperada determinista $E[C\_n] = u + n(1 - E[Y])$, ilustrando la pendiente de deriva $1 - E[Y]$.

### Pestaña 2: Recursión vs. Simulación
Contrasta para cada capital inicial $u \in \\{0, 1, \dots, u\_{\max}\\}$:
* **Barras azules:** Probabilidad exacta $\psi(u)$ obtenida mediante la fórmula recursiva (Proposición 7.1):

$$\psi(0) = E[Y], \qquad \psi(u) = \frac{1}{f(0)}\left[\sum\_{y=u}^{\infty} \bar{F}(y) + \sum\_{y=1}^{u-1} \psi(u-y)\bar{F}(y)\right], \quad u \ge 1,$$

donde $\bar{F}(y) = P(Y > y) = 1 - F(y)$.
* **Puntos rojos con barras de error:** Proporción empírica $\hat{\psi}(u)$ obtenida por Monte Carlo junto con su intervalo de confianza del $95\%$: $\hat{\psi}(u) \pm 1.96\sqrt{\hat{\psi}(u)(1-\hat{\psi}(u))/n\_{\text{sim}}}$.

### Pestaña 3: Ley Grandes Números
Ilustra la convergencia casi segura enunciada en la Proposición 7.2:

$$\frac{C\_n}{n} = \frac{u}{n} + 1 - \frac{1}{n}\sum\_{j=1}^n Y\_j \xrightarrow[n \to \infty]{\text{c.s.}} 1 - E[Y].$$

Se proyectan $12$ trayectorias extendidas a lo largo de $\max(200, 3N)$ periodos para visualizar cómo los promedios temporales $C\_n/n$ convergen hacia la línea discontinua roja $1 - E[Y]$. Cuando $1 - E[Y] > 0$, se comprueba visualmente que $C\_n \to +\infty$ y por ende $\lim\_{u \to \infty} \psi(u) = 0$.

### Pestaña 4: Calculadora Recursiva
Muestra dos tablas numéricas de alta precisión:
* **Distribución de Reclamación $Y$:** Lista los valores $y$, la probabilidad puntual $f(y)$, la acumulada $F(y)$ y la función de cola $\bar{F}(y) = 1 - F(y)$.
* **Probabilidades de Ruina $\psi(u)$:** Reporta para cada $u \in \\{0, \dots, u\_{\max}\\}$ el valor exacto por recursión $\psi(u)$, la fórmula cerrada explícita (disponible para *Ejemplo 7.2*, *Ejemplo 7.3* y *Bernoulli*), el estimador simulado $\hat{\psi}(u)$ y el error absoluto $|\psi(u) - \hat{\psi}(u)|$.

### Pestaña 5: Tiempo ($\tau$) y Severidad
Analiza la distribución condicional de las carteras que efectivamente llegaron a la ruina ($\tau \le N$), siempre que ocurran al menos $5$ quiebras en la muestra simulada:
* **Histograma izquierdo ($\tau \mid \tau \le N$):** Muestra los periodos en que ocurren las quiebras y marca con una línea vertical discontinua el tiempo medio condicional $E[\tau \mid \tau \le N]$.
* **Gráfica derecha ($-C\_\tau \mid \tau \le N$):** Muestra la distribución de la **severidad de la ruina** (el déficit $-C\_\tau \ge 0$ en el instante exacto de quiebra) junto con el déficit promedio.

---

## 5. Ejemplos Prácticos de Uso en Clase

1. **Caso base del libro (Configuración por defecto):**
   * Mantener *Ejemplo 7.1* ($f(0)=0.5, f(1)=0.2, f(2)=0.3, f(3)=0$), $u=3$, $N=35$.
   * En **Vista 1**, presionar **Paso +1** para mostrar cómo $Y\_n=0$ sube $+1$, $Y\_n=1$ mantiene el capital sin cambio y $Y\_n=2$ lo reduce en $-1$. Luego usar **Ruta con ruina** y **Reproducir** para observar una quiebra.
   * En **Vista 2**, comprobar que $E[Y]=0.8000$, $\psi(0)=0.8000$ y $\psi(3)=0.2160$ ($21.60\%$).
2. **Ruina del Jugador y contraste con fórmula cerrada:**
   * Elegir *Ejemplo 7.2: Ruina del Jugador (0 o 2)* con $p = 0.65$ y $u = 4$.
   * En la **Pestaña 4**, verificar que los valores recursivos coinciden a seis decimales con $\psi(u) = \left(\frac{1-p}{p}\right)^u = \left(\frac{0.35}{0.65}\right)^u$ para $u \ge 1$.
3. **Caso límite Bernoulli($p$):**
   * Elegir *Bernoulli($p$)* con $p = 0.40$ y $u = 1$.
   * Observar que como las reclamaciones solo valen $0$ o $1$, el incremento neto $1 - Y\_j \in \\{0, 1\\}$ nunca es negativo: por ello $\psi(0) = 0.40$ pero $\psi(u) = 0$ para todo $u \ge 1$.
4. **Sobredispersión con Binomial Negativa($r, p$):**
   * Seleccionar *Binomial Negativa($r, p$)* con $r = 2$ y $p = 0.75$ ($E[Y] \approx 0.6667$, igual media que el *Ejemplo 7.3* con $p = 0.60$).
   * Comparar las curvas de $\psi(u)$ en la **Pestaña 2** y la severidad en la **Pestaña 5** para apreciar cómo la mayor varianza de las reclamaciones incrementa la probabilidad y la severidad de la ruina aun teniendo la misma esperanza $E[Y]$.
5. **Violación de ganancia neta (Ruina segura):**
   * Elegir *Poisson($\lambda$)* y deslizar $\lambda$ hasta $1.10$ (de modo que $E[Y] = 1.10 > 1$).
   * Verificar que el indicador superior cambia a rojo (*NO CUMPLE (Ruina segura)*), que $\psi(u) = 1$ para todo $u$ en la **Pestaña 2** y que $C\_n / n \to -0.10 < 0$ en la **Pestaña 3**.
6. **Distribución Personalizada con siniestros severos poco frecuentes:**
   * Elegir *Personalizada (Valores y Prob. libres)* con $k = 3$ valores: $(y\_1 = 0, f(0) = 0.85)$, $(y\_2 = 1, f(1) = 0.10)$ y $(y\_3 = 6, f(6) = 0.05)$.
   * Verificar que $E[Y] = 0.40 < 1$ (ganancia neta favorable), pero al observar la **Vista 1** o la **Pestaña 5**, un solo siniestro catastrófico de $6$ unidades puede provocar una ruina abrupta con alta severidad.