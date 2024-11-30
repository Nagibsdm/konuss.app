import streamlit as st
import pandas as pd
import matplotlib.pyplot as plt

# Configuración de los mercados y productos
mercados = {
    "Bodegones": {
        "Pizza en Cono": {"precio_venta": 4.70, "costo_variable": 1.60, "escenarios": {"pesimista": 4000, "base": 6000, "optimista": 10000}},
        "Panzerotti": {"precio_venta": 4.80, "costo_variable": 1.80, "escenarios": {"pesimista": 400, "base": 533, "optimista": 667}},  # Paquete de 6
        "Pastelitos": {"precio_venta": 0.50, "costo_variable": 0.20, "escenarios": {"pesimista": 33000, "base": 35000, "optimista": 40000}},
    },
    "Cantinas": {
        "Pizza en Cono Pequeños": {"precio_venta": 4.00, "costo_variable": 1.40, "escenarios": {"pesimista": 3000, "base": 5000, "optimista": 7000}},
        "Panzerotti": {"precio_venta": 4.80, "costo_variable": 1.80, "escenarios": {"pesimista": 333, "base": 500, "optimista": 667}},  # Paquete de 6
        "Pastelitos": {"precio_venta": 0.60, "costo_variable": 0.25, "escenarios": {"pesimista": 31000, "base": 33000, "optimista": 36000}},
    },
    "Delivery y Take Away": {
        "Pizza en Cono": {"precio_venta": 5.00, "costo_variable": 1.80, "escenarios": {"pesimista": 4000, "base": 6000, "optimista": 10000}},
        "Panzerotti": {"precio_venta": 4.80, "costo_variable": 1.80, "escenarios": {"pesimista": 367, "base": 533, "optimista": 700}},  # Paquete de 6
        "Pastelitos": {"precio_venta": 0.55, "costo_variable": 0.20, "escenarios": {"pesimista": 33000, "base": 34000, "optimista": 38000}},
    },
}

# Gasto fijo total para toda la producción
gasto_fijo_total = 2500

# Función para distribuir el gasto fijo proporcionalmente
def distribuir_gasto_fijo(productos):
    for mercado, productos_mercado in productos.items():
        for producto, datos in productos_mercado.items():
            total_produccion = sum(datos["escenarios"].values())  # Total de la producción de todos los escenarios
            for escenario, cantidad in datos["escenarios"].items():
                datos["gasto_fijo_proporcional"] = (cantidad / total_produccion) * gasto_fijo_total
    return productos

# Distribuir el gasto fijo a todos los productos
mercados = distribuir_gasto_fijo(mercados)

# Función para calcular los resultados mensuales
def calcular_resultados_mensuales(productos):
    resultados = []
    for producto, datos in productos.items():
        for escenario, cantidad in datos["escenarios"].items():
            ingreso = cantidad * datos["precio_venta"]
            costo_variable_total = cantidad * datos["costo_variable"]
            margen_contribucion = ingreso - costo_variable_total
            utilidad_operativa = margen_contribucion - datos["gasto_fijo_proporcional"]
            rentabilidad = (utilidad_operativa / ingreso * 100) if ingreso > 0 else 0
            ganancia_neta = ingreso - costo_variable_total - datos["gasto_fijo_proporcional"]

            resultados.append({
                "Producto": producto,
                "Escenario": escenario.capitalize(),
                "Ingresos Mensuales ($)": ingreso,
                "Costos Variables Mensuales ($)": costo_variable_total,
                "Margen de Contribución ($)": margen_contribucion,
                "Utilidad Operativa Mensual ($)": utilidad_operativa,
                "Rentabilidad (%)": rentabilidad,
                "Ganancia Neta Mensual ($)": ganancia_neta
            })
    return pd.DataFrame(resultados)

# Función para mostrar gráfico de barras
def mostrar_grafico_configuracion_barras(productos, escenario):
    fig, ax = plt.subplots(figsize=(12, 6))

    nombres = []
    ingresos = []
    costos_variables = []
    gastos_fijos = []
    ganancias_netas = []

    # Calcular datos para cada producto en función del escenario seleccionado
    for producto, datos in productos.items():
        nombres.append(producto)
        ingreso = datos["escenarios"][escenario] * datos["precio_venta"]
        costo_variable = datos["escenarios"][escenario] * datos["costo_variable"]
        gasto_fijo = datos["gasto_fijo_proporcional"]
        ganancia_neta = ingreso - costo_variable - gasto_fijo

        ingresos.append(ingreso)
        costos_variables.append(costo_variable)
        gastos_fijos.append(gasto_fijo)
        ganancias_netas.append(ganancia_neta)

    # Crear barras
    ax.bar(nombres, ingresos, label="Ingresos ($)", color="#4CAF50")
    ax.bar(nombres, costos_variables, label="Costos Variables ($)", color="#FF5733")
    ax.bar(nombres, gastos_fijos, bottom=costos_variables, label="Gastos Fijos ($)", color="#3498DB")

    # Mostrar ganancias netas como texto en el gráfico
    for i, ganancia in enumerate(ganancias_netas):
        ax.text(i, ingresos[i] + 200, f"Ganancia: ${ganancia:,.2f}", ha="center", fontsize=10, color="black", bbox=dict(facecolor='white', alpha=0.7))

    # Configuración del gráfico
    ax.set_title(f"Ventas, Costos y Ganancias Netas por Producto ({escenario.capitalize()})", fontsize=16, fontweight="bold")
    ax.set_ylabel("Monto ($)")
    ax.set_xlabel("Productos")
    ax.legend()
    ax.grid(axis="y", linestyle="--", alpha=0.7)

    st.pyplot(fig)

# Función para actualizar automáticamente los valores de los productos
mercado_seleccionado = st.sidebar.selectbox("Seleccione el Mercado", list(mercados.keys()))

for mercado, productos_mercado in mercados.items():
    if mercado == mercado_seleccionado:
        for producto, datos in productos_mercado.items():
            with st.sidebar.expander(f"⚙️ Configuración de {producto} ({mercado})"):
                datos["precio_venta"] = st.number_input(f"Precio de venta ({producto})", value=datos["precio_venta"], key=f"precio_venta_{mercado}_{producto}")
                datos["costo_variable"] = st.number_input(f"Costo variable ({producto})", value=datos["costo_variable"], key=f"costo_variable_{mercado}_{producto}")
                datos["escenarios"]["pesimista"] = st.slider(f"Escenario Pesimista ({producto})", 0, 50000, datos["escenarios"]["pesimista"], step=500, key=f"pesimista_{mercado}_{producto}")
                datos["escenarios"]["base"] = st.slider(f"Escenario Base ({producto})", 0, 50000, datos["escenarios"]["base"], step=500, key=f"base_{mercado}_{producto}")
                datos["escenarios"]["optimista"] = st.slider(f"Escenario Optimista ({producto})", 0, 50000, datos["escenarios"]["optimista"], step=500, key=f"optimista_{mercado}_{producto}")

# Selección de escenario para el gráfico
st.header("Visualización de Ventas y Costos")
escenario_grafico = st.selectbox("Selecciona un escenario para visualizar:", ["pesimista", "base", "optimista"])

productos = mercados[mercado_seleccionado]  # Filtrar productos según el mercado seleccionado
mostrar_grafico_configuracion_barras(productos, escenario_grafico)

# Calcular y mostrar resultados financieros
st.header("Resultados Financieros")
escenario_filtrado = st.selectbox("Filtrar resultados por escenario:", ["Pesimista", "Base", "Optimista"])
resultados_df = calcular_resultados_mensuales(productos)
resultados_df = resultados_df[resultados_df["Escenario"] == escenario_filtrado]
st.dataframe(resultados_df)

# Resumen consolidado
st.subheader("Resumen Consolidado")
totales = resultados_df.groupby("Escenario").sum()
totales = totales[["Ingresos Mensuales ($)", "Costos Variables Mensuales ($)", "Ganancia Neta Mensual ($)"]]
st.dataframe(totales)
