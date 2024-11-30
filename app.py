import streamlit as st
import pandas as pd
import matplotlib.pyplot as plt
import json

# Configuración inicial de los mercados y productos
mercados_iniciales = {
    "Bodegones": {
        "Pizza en Cono": {"precio_venta": 4.70, "costo_variable": 1.60, "escenarios": {"pesimista": 4000, "base": 6000, "optimista": 10000}},
        "Panzerotti": {"precio_venta": 4.80, "costo_variable": 1.80, "escenarios": {"pesimista": 400, "base": 533, "optimista": 667}},
        "Pastelitos": {"precio_venta": 0.50, "costo_variable": 0.20, "escenarios": {"pesimista": 3300, "base": 3500, "optimista": 4000}},
    },
    "Cantinas": {
        "Pizza en Cono Pequeños": {"precio_venta": 4.00, "costo_variable": 1.40, "escenarios": {"pesimista": 3000, "base": 5000, "optimista": 7000}},
        "Panzerotti": {"precio_venta": 4.80, "costo_variable": 1.80, "escenarios": {"pesimista": 333, "base": 500, "optimista": 667}},
        "Pastelitos": {"precio_venta": 0.60, "costo_variable": 0.25, "escenarios": {"pesimista": 3100, "base": 3300, "optimista": 3600}},
    },
}

# Utilizar session_state para mantener los cambios entre mercados
if "mercados" not in st.session_state:
    st.session_state.mercados = mercados_iniciales

# Ajustar gasto fijo total desde un cuadro
gasto_fijo_total = st.sidebar.number_input("Total Gasto Fijo ($):", value=2500, step=100)

# Selección del mercado desde la barra lateral
mercado_seleccionado = st.sidebar.selectbox("Seleccione un mercado:", list(st.session_state.mercados.keys()) + ["Total"])

# Selección del escenario desde la barra lateral
escenario_seleccionado = st.sidebar.selectbox("Seleccione un escenario:", ["pesimista", "base", "optimista"])

# Función para distribuir el gasto fijo proporcionalmente entre mercados y productos
def distribuir_gasto_fijo(mercados, gasto_fijo_total):
    total_produccion = sum(
        sum(sum(datos["escenarios"].values()) for datos in mercados[mercado].values())
        for mercado in mercados.keys()
    )

    for mercado in mercados.keys():
        for producto, datos in mercados[mercado].items():
            total_producto = sum(datos["escenarios"].values())
            datos["gasto_fijo_proporcional"] = gasto_fijo_total * (total_producto / total_produccion)
    return mercados

# Distribuir el gasto fijo
st.session_state.mercados = distribuir_gasto_fijo(st.session_state.mercados, gasto_fijo_total)

# Función para guardar la configuración en un archivo JSON
def guardar_configuracion(mercados, gasto_fijo_total):
    with open("configuracion.json", "w") as file:
        json.dump({"mercados": mercados, "gasto_fijo_total": gasto_fijo_total}, file)
    st.sidebar.success("Configuración guardada correctamente.")

# Función para cargar la configuración desde un archivo JSON
def cargar_configuracion():
    try:
        with open("configuracion.json", "r") as file:
            data = json.load(file)
        st.sidebar.success("Configuración cargada correctamente.")
        return data["mercados"], data["gasto_fijo_total"]
    except FileNotFoundError:
        st.sidebar.error("No se encontró una configuración guardada.")
        return st.session_state.mercados, gasto_fijo_total

# Botones para guardar y cargar configuración
if st.sidebar.button("Guardar Configuración"):
    guardar_configuracion(st.session_state.mercados, gasto_fijo_total)

if st.sidebar.button("Cargar Configuración"):
    st.session_state.mercados, gasto_fijo_total = cargar_configuracion()
    st.session_state.mercados = distribuir_gasto_fijo(st.session_state.mercados, gasto_fijo_total)

# Configuración interactiva de productos
if mercado_seleccionado != "Total":
    st.sidebar.header(f"Configuración de {mercado_seleccionado}")
    productos = st.session_state.mercados[mercado_seleccionado]
    for idx, (producto, datos) in enumerate(productos.items()):
        with st.sidebar.expander(f"⚙️ {producto}"):
            # Ajustar el precio de venta
            datos["precio_venta"] = st.number_input(
                f"Precio de venta ({producto})", 
                value=datos["precio_venta"], 
                step=0.1, 
                key=f"precio_{producto}_{mercado_seleccionado}_{idx}"
            )
            # Ajustar el costo variable
            datos["costo_variable"] = st.number_input(
                f"Costo variable ({producto})", 
                value=datos["costo_variable"], 
                step=0.1, 
                key=f"costo_{producto}_{mercado_seleccionado}_{idx}"
            )
            # Ajustar las cantidades para cada escenario
            for escenario in ["pesimista", "base", "optimista"]:
                datos["escenarios"][escenario] = st.number_input(
                    f"Cantidad {escenario.capitalize()} ({producto})", 
                    value=datos["escenarios"][escenario], 
                    step=100, 
                    min_value=0, 
                    max_value=15000, 
                    key=f"{escenario}_{producto}_{mercado_seleccionado}_{idx}"
                )

# Función para calcular resultados financieros
def calcular_resultados_financieros(mercado, productos, escenario):
    resultados = []
    for producto, datos in productos.items():
        cantidad = datos["escenarios"][escenario]
        ingreso = cantidad * datos["precio_venta"]
        costo_variable_total = cantidad * datos["costo_variable"]
        gasto_fijo = datos.get("gasto_fijo_proporcional", 0)
        ganancia_neta = ingreso - costo_variable_total - gasto_fijo
        resultados.append({
            "Mercado": mercado,
            "Producto": producto,
            "Cantidad Vendida": cantidad,
            "Ingresos ($)": ingreso,
            "Costos Variables ($)": costo_variable_total,
            "Gasto Fijo Proporcional ($)": gasto_fijo,
            "Ganancia Neta ($)": ganancia_neta,
        })
    return resultados

# Consolidar resultados para "Total"
def consolidar_resultados(mercados, escenario):
    resultados_totales = {}
    for mercado, productos in mercados.items():
        for resultado in calcular_resultados_financieros(mercado, productos, escenario):
            producto = resultado["Producto"]
            if producto not in resultados_totales:
                resultados_totales[producto] = resultado
            else:
                # Sumar los valores para consolidar productos del mismo tipo
                resultados_totales[producto]["Cantidad Vendida"] += resultado["Cantidad Vendida"]
                resultados_totales[producto]["Ingresos ($)"] += resultado["Ingresos ($)"]
                resultados_totales[producto]["Costos Variables ($)"] += resultado["Costos Variables ($)"]
                resultados_totales[producto]["Gasto Fijo Proporcional ($)"] += resultado["Gasto Fijo Proporcional ($)"]
                resultados_totales[producto]["Ganancia Neta ($)"] += resultado["Ganancia Neta ($)"]
    return list(resultados_totales.values())

# Calcular resultados financieros para el mercado seleccionado
if mercado_seleccionado == "Total":
    resultados = consolidar_resultados(st.session_state.mercados, escenario_seleccionado)
else:
    productos = st.session_state.mercados[mercado_seleccionado]
    resultados = calcular_resultados_financieros(mercado_seleccionado, productos, escenario_seleccionado)

# Mostrar resultados financieros
st.header(f"Resultados Financieros - {mercado_seleccionado} ({escenario_seleccionado.capitalize()})")
resultados_df = pd.DataFrame(resultados)

# Agregar fila de totales al final de la tabla
totales = resultados_df.sum(numeric_only=True).to_dict()
totales["Producto"] = "TOTAL"
totales["Mercado"] = ""
resultados_df = pd.concat([resultados_df, pd.DataFrame([totales])], ignore_index=True)

st.dataframe(resultados_df)

# Opciones para el gráfico
metricas_grafico = st.multiselect(
    "Seleccione las métricas para el gráfico:",
    ["Ingresos ($)", "Costos Variables ($)", "Gasto Fijo Proporcional ($)", "Ganancia Neta ($)"],
    default=["Ingresos ($)", "Costos Variables ($)", "Gasto Fijo Proporcional ($)"]
)

# Función para mostrar gráfico de barras
def mostrar_grafico_barras(resultados, metricas):
    fig, ax = plt.subplots(figsize=(10, 6))
    productos = [resultado["Producto"] for resultado in resultados]
    colores = {"Ingresos ($)": "#4CAF50", "Costos Variables ($)": "#FF5733", "Gasto Fijo Proporcional ($)": "#3498DB", "Ganancia Neta ($)": "#FFC300"}
    valores = {metrica: [resultado[metrica] for resultado in resultados] for metrica in metricas}

    bottom = [0] * len(productos)
    for metrica in metricas:
        ax.bar(productos, valores[metrica], bottom=bottom, label=metrica, color=colores[metrica])
        bottom = [b + v for b, v in zip(bottom, valores[metrica])]

    ax.set_title("Ingresos, Costos y Ganancia Neta por Producto")
    ax.set_ylabel("Monto ($)")
    ax.legend()
    st.pyplot(fig)

# Mostrar gráfico
st.subheader("Gráfico de Ingresos, Costos y Ganancia Neta")
mostrar_grafico_barras(resultados, metricas_grafico)
