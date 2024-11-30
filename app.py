import streamlit as st
import pandas as pd
import json
import matplotlib.pyplot as plt

# Función para calcular resultados mensuales
def calcular_resultados_mensuales(productos):
    resultados = []
    for producto, datos in productos.items():
        for escenario, cantidad in datos["escenarios"].items():
            ingreso = cantidad * datos["precio_venta"]
            costo_variable_total = cantidad * datos["costo_variable"]
            margen_contribucion = ingreso - costo_variable_total
            utilidad_operativa = margen_contribucion - datos["gastos_fijos"]
            rentabilidad = (utilidad_operativa / ingreso * 100) if ingreso > 0 else 0

            resultados.append({
                "Producto": producto,
                "Escenario": escenario.capitalize(),
                "Ingresos Mensuales ($)": ingreso,
                "Costos Variables Mensuales ($)": costo_variable_total,
                "Margen de Contribución ($)": margen_contribucion,
                "Utilidad Operativa Mensual ($)": utilidad_operativa,
                "Rentabilidad (%)": rentabilidad,
            })
    return pd.DataFrame(resultados)

# Función para mostrar gráfico de costos y ventas
def mostrar_grafico_configuracion(productos):
    fig, ax = plt.subplots(figsize=(10, 5))
    nombres = []
    ventas_estimadas = []
    costos_variables_totales = []
    for producto, datos in productos.items():
        nombres.append(producto)
        ventas_estimadas.append(datos["escenarios"]["base"] * datos["precio_venta"])
        costos_variables_totales.append(datos["escenarios"]["base"] * datos["costo_variable"])

    ax.bar(nombres, ventas_estimadas, color="#A3D6A8", label="Ventas Estimadas (Base)")
    ax.bar(nombres, costos_variables_totales, color="#FF9999", label="Costos Variables (Base)")
    ax.set_title("Configuración de Ventas y Costos", fontsize=16, fontweight="bold")
    ax.legend()
    st.pyplot(fig)

# Función para guardar configuración
def guardar_configuracion(productos):
    with open("configuracion.json", "w") as file:
        json.dump(productos, file)
    st.sidebar.success("Configuración guardada.")

# Función para cargar configuración
def cargar_configuracion():
    try:
        with open("configuracion.json", "r") as file:
            productos = json.load(file)
        st.sidebar.success("Configuración cargada.")
        return productos
    except FileNotFoundError:
        st.sidebar.error("No se encontró un archivo de configuración.")
        return None

# Configuración inicial de los productos
productos = {
    "Pizzas en Cono": {
        "precio_venta": 2.35,
        "costo_variable": 0.8,
        "gastos_fijos": 1400,
        "escenarios": {"pesimista": 2000, "base": 3000, "optimista": 5000},
    },
    "Pastelitos": {
        "precio_venta": 0.5,
        "costo_variable": 0.17,
        "gastos_fijos": 0,
        "escenarios": {"pesimista": 33000, "base": 33000, "optimista": 33000},
    },
    "Donas": {
        "precio_venta": 1.0,
        "costo_variable": 0.4,
        "gastos_fijos": 0,
        "escenarios": {"pesimista": 4000, "base": 4000, "optimista": 4000},
    },
    "Panzerotti Grandes": {
        "precio_venta": 1.0,
        "costo_variable": 0.4,
        "gastos_fijos": 0,
        "escenarios": {"pesimista": 2400, "base": 2400, "optimista": 2400},
    },
    "Panzerotti Pequeños": {
        "precio_venta": 4.2,
        "costo_variable": 1.2,
        "gastos_fijos": 0,
        "escenarios": {"pesimista": 2000, "base": 3000, "optimista": 5000},
    },
}

# Mostrar el configurador interactivo
st.title("Configurador de Productos 📊")
st.sidebar.header("Opciones del Configurador")

# Botones para guardar, cargar y resetear configuraciones
if st.sidebar.button("Guardar Configuración"):
    guardar_configuracion(productos)

if st.sidebar.button("Cargar Configuración"):
    productos_cargados = cargar_configuracion()
    if productos_cargados:
        productos = productos_cargados

if st.sidebar.button("Resetear Configuración"):
    st.experimental_rerun()

# Configuración por producto (usando expansores)
for producto, datos in productos.items():
    with st.sidebar.expander(f"⚙️ Configuración: {producto}"):
        datos["precio_venta"] = st.number_input(f"Precio venta ({producto})", value=datos["precio_venta"])
        datos["costo_variable"] = st.number_input(f"Costo variable ({producto})", value=datos["costo_variable"])
        datos["gastos_fijos"] = st.number_input(f"Gastos fijos ({producto})", value=datos["gastos_fijos"])
        datos["escenarios"]["pesimista"] = st.slider(f"Escenario Pesimista ({producto})", 0, 50000, datos["escenarios"]["pesimista"], step=500)
        datos["escenarios"]["base"] = st.slider(f"Escenario Base ({producto})", 0, 50000, datos["escenarios"]["base"], step=500)
        datos["escenarios"]["optimista"] = st.slider(f"Escenario Optimista ({producto})", 0, 50000, datos["escenarios"]["optimista"], step=500)

# Mostrar un resumen de configuración filtrado por escenario
st.header("Resumen de Configuración")
escenario_config_filtrado = st.selectbox("Filtrar resumen de configuración por escenario:", ["Pesimista", "Base", "Optimista"])
config_resumen = []
for producto, datos in productos.items():
    ventas_filtradas = datos["escenarios"][escenario_config_filtrado.lower()]
    config_resumen.append({
        "Producto": producto,
        "Precio Venta ($)": f"${datos['precio_venta']:,.2f}",
        "Costo Variable ($)": f"${datos['costo_variable']:,.2f}",
        "Gastos Fijos ($)": f"${datos['gastos_fijos']:,.2f}",
        "Ventas Estimadas": f"{ventas_filtradas:,}",
    })
resumen_df = pd.DataFrame(config_resumen)
st.dataframe(resumen_df)

# Mostrar el gráfico de configuración
st.header("Visualización de Ventas y Costos")
mostrar_grafico_configuracion(productos)

# Calcular resultados y mostrar las tablas
st.header("Resultados Financieros")

# Selección de escenario para filtrado de resultados
escenario_filtrado = st.selectbox("Filtrar resultados detallados por escenario:", ["Todos", "Pesimista", "Base", "Optimista"])
resultados_df = calcular_resultados_mensuales(productos)

# Filtrar resultados si se selecciona un escenario específico
if escenario_filtrado != "Todos":
    resultados_df = resultados_df[resultados_df["Escenario"] == escenario_filtrado]

st.subheader("Resultados Detallados")
st.dataframe(resultados_df)

# Resumen consolidado por escenario
st.subheader("Resumen Consolidado")
totales = resultados_df.groupby("Escenario").sum(numeric_only=True)
totales["Rentabilidad (%)"] = (totales["Utilidad Operativa Mensual ($)"] / totales["Ingresos Mensuales ($)"]) * 100
totales = totales.drop(columns=["Utilidad Operativa Mensual ($)", "Margen de Contribución ($)", "Costos Variables Mensuales ($)"])
totales_formateados = totales.style.format({"Ingresos Mensuales ($)": "${:,.2f}", "Rentabilidad (%)": "{:.2f}%"})
st.dataframe(totales_formateados)
