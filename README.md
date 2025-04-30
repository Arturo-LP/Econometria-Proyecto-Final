# Inflación a Nivel Hogar

Proyecto Econométrico (CIDE-ME, 2025): Experiencia Inflacionaria a Nivel Hogar
  
## Metodología

Con el objetivo de capturar la heterogeneidad en las experiencias inflacionarias de los hogares, construimos un **índice de inflación individualizado** para cada hogar a partir de la Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH) y el Índice Nacional de Precios al Consumidor (INPC) desagregado por rubro y entidad federativa.

El procedimiento consiste en ponderar las tasas de inflación por rubro según la estructura de gasto observada a nivel hogar. Formalmente, para cada hogar $$h$$ en el periodo $$t$$, el índice de inflación individualizada se define como:

$$
\pi_{h, t, t-k} = \sum_{i=1}^{n} w_{h,i} \cdot \pi_{i, t, t-k}^{e}
$$

donde:

- $$\pi_{h, t, t-k}$$: Tasa de inflación individualizada del hogar $$h$$ en el periodo $$t$$ con periodo base $$t-k$$
- $$w_{h,i}$$: Participación del rubro $$i$$ en el gasto total del hogar $$h$$
- $$\pi_{i, t, t-k}^{e}$$: Tasa de inflación del rubro $$i$$ en la entidad federativa $$e$$ donde reside el hogar $$h$$ en el periodo $$t$$ con periodo base $$t-k$$

Cada vector de ponderaciones $$w_{h} = (w_{h,1}, w_{h,2}, \dots, w_{h,n})$$ fue construido a partir de los gastos trimestrales reportados por hogar en los rubros disponibles en ENIGH, normalizados como proporciones del gasto total.

Por su parte, las tasas de inflación $$\pi_{i, t, t-k}^{e}$$ fueron obtenidas a partir del INPC publicado por el INEGI, considerando la desagregación regional y por rubro de gasto.

Este índice permite observar cómo varía la inflación experimentada entre hogares con distintos niveles de ingreso, estructura de consumo y ubicación geográfica, superando las limitaciones del enfoque tradicional que utiliza un índice de precios promedio nacional y homogéneo.


## Análisis Econométrico Preliminar

Como parte del análisis de las implicaciones distributivas de la inflación, proponemos estimar una regresión cuantílica que relacione la inflación individualizada de los hogares con sus características observables. Este enfoque permite capturar cómo varían los determinantes de la inflación experimentada a lo largo de la distribución condicional.

La especificación general del modelo a estimar es:

$$
Q_\tau(g_{h,t} \mid \pi_{h, t, t-k}, X_{h,t}) = \alpha_\tau + \delta_\tau \pi_{h, t, t-k} + X_{h,t}'\beta_\tau + \epsilon_\tau
$$

donde:

- $$g_{h,t}$$: Gasto monetario trimestral del hogar $$h$$ en el periodo $$t$$
- $$\pi_{h, t, t-k}$$: Tasa de inflación individualizada del hogar $$h$$ en el periodo $$t$$ con base $$t-k$$
- $$X_{h,t}$$: Vector de controles observables del hogar
- $$\delta_\tau$$: Coeficiente que captura el efecto de la inflación sobre el gasto en el cuantil $$\tau$$
- $$\beta_\tau$$: Coeficientes específicos al cuantil $$\tau$$ para las covariables
- $$Q_\tau(\cdot)$$: Cuantil condicional de orden $$\tau \in (0,1)$$

Este enfoque permite evaluar si la sensibilidad del gasto frente a la inflación varía a lo largo de su distribución —por ejemplo, si los hogares con menor gasto reducen más su consumo ante mayores tasas de inflación que los hogares con mayor gasto. Esto proporciona evidencia sobre el potencial efecto regresivo de la inflación, especialmente en regímenes de alta inflación.


## Revisión de Literatura

**Inflación y patrones de consumo diferenciados**

- *Attanasio y Pistaferri (2016)* documentan heterogeneidad en patrones de consumo asociada a características socioeconómicas.
- *Kaplan y Schulhofer-Wohl (2017)* identifican efectos inflacionarios diferentes según el tipo de canasta de consumo por nivel de ingreso.
- *Shaoyu et al. (2017)* señalan que las diferencias en patrones de gasto y en aumentos de precios conducen a experiencias inflacionarias desiguales entre hogares (*desigualdad inflacionaria*).
- *Cravino, Lan y Levchenko (2020)* encuentran que los precios de bienes consumidos por hogares de altos ingresos son más rígidos y menos volátiles.
- *Jaravel (2021)* documenta diferencias en tasas de inflación experimentadas por distintos grupos socioeconómicos.
- *Lauper y Mangiante (2023)* muestran que los hogares de ingresos bajos y medios enfrentan tasas de inflación más altas y mayor sensibilidad a políticas monetarias contractivas. Un efecto similar es observado al analizar grupos clasificados por nivel de gasto y salario.
- *Kaplan y Violante (2018)* argumentan que la heterogeneidad microeconómica amplifica o atenúa los shocks macroeconómicos.

  
## Referencias

Attanasio, O. P., & Pistaferri, L. (2016). *Consumption inequality*. Journal of Economic Perspectives, 30(2), 3–28.

Coibion, O., Gorodnichenko, Y., & Weber, M. (2019). *Household inflation expectations and consumer spending: Evidence from panel data*. NBER Working Paper No. 25844. National Bureau of Economic Research.

Cravino, J., Lan, T., & Levchenko, A. A. (2020). *Price stickiness along the income distribution and the effects of monetary policy*. Journal of Monetary Economics, 110, 19–32.

Jaravel, X. (2021). *Inflation inequality: Measurement, causes, and policy implications*. Annual Review of Economics, 13, 599–629.

Kaplan, G., & Schulhofer-Wohl, S. (2017). *Inflation at the household level*. Journal of Monetary Economics, 91, 19–38. https://doi.org/10.1016/j.jmoneco.2017.08.002

Kaplan, G., & Violante, G. L. (2018). *Microeconomic heterogeneity and macroeconomic shocks*. Journal of Economic Perspectives, 32(3), 167–194. https://doi.org/10.1257/jep.32.3.167

Lauper, C., & Mangiante, G. (2023). *Monetary policy shocks and inflation inequality*. SSRN Electronic Journal. https://doi.org/10.2139/ssrn.4409096

Shaoyu, L., Lijia, W., & Zhiwei, X. (2017). *Dynamic asset allocation and consumption under inflation inequality: The impacts of inflation experiences and expectations*. Economic Modelling, 61, 113–125. https://doi.org/10.1016/j.econmod.2016.11.013
