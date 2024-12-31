import streamlit as st
import pandas as pd
import os
import plotly.express as px

# --- Configuración de la página ---
st.set_page_config(page_title="SDM Advisory - Análisis Financiero", layout="wide")

# --- Función para guardar datos en un archivo CSV ---
def guardar_csv(data, filename):
    df = pd.DataFrame(data)
    df.to_csv(filename, index=False)
    st.success(f"Archivo guardado exitosamente: {filename}")

# --- Función para cargar archivo CSV ---
def cargar_csv(filename):
    if os.path.exists(filename):
        return pd.read_csv(filename)
    return pd.DataFrame()

# --- Función para validar archivos ---
def validar_columnas(df, columnas_requeridas):
    for columna in columnas_requeridas:
        if columna not in df.columns:
            st.error(f"El archivo debe contener las siguientes columnas: {', '.join(columnas_requeridas)}.")
            return False
    return True

# --- Sección de Flujo de Caja ---
def seccion_flujo_caja():
    st.header("Análisis de Flujo de Caja")
    filename = "flujo_caja.csv"
    columnas = ["Producto", "Ingresos", "Costos"]

    # Inicializar datos
    df = cargar_csv(filename)
    data = df.to_dict(orient="list") if not df.empty else {"Producto": [], "Ingresos": [], "Costos": []}

    # Entrada de datos manual
    num_productos = st.number_input("Cantidad de Productos a Agregar:", min_value=1, step=1, value=1, key="flujo_num_productos")

    for i in range(int(num_productos)):
        st.subheader(f"Producto {i+1}")
        producto = st.text_input(f"Nombre del Producto {i+1}", key=f"flujo_producto_{i}")
        ingreso = st.number_input(f"Ingresos del Producto {i+1} ($)", min_value=0.0, key=f"flujo_ingreso_{i}")
        costo = st.number_input(f"Costos del Producto {i+1} ($)", min_value=0.0, key=f"flujo_costo_{i}")
        data["Producto"].append(producto)
        data["Ingresos"].append(ingreso)
        data["Costos"].append(costo)

    # Guardar datos
    if st.button("Guardar Flujo de Caja"):
        guardar_csv(data, filename)

    # Visualizar datos y gráficos
    df = pd.DataFrame(data)
    st.subheader("Datos del Flujo de Caja")
    st.dataframe(df)

    df["Flujo Neto"] = df["Ingresos"] - df["Costos"]
    fig = px.bar(df, x="Producto", y=["Ingresos", "Costos", "Flujo Neto"], title="Flujo de Caja")
    st.plotly_chart(fig)

# --- Sección de Rentabilidad ---
def seccion_rentabilidad():
    st.header("Análisis de Rentabilidad")
    filename = "rentabilidad.csv"
    columnas = ["Producto", "Ingresos", "Costos Fijos", "Costos Variables"]

    # Inicializar datos
    df = cargar_csv(filename)
    data = df.to_dict(orient="list") if not df.empty else {"Producto": [], "Ingresos": [], "Costos Fijos": [], "Costos Variables": []}

    # Entrada de datos manual
    num_productos = st.number_input("Cantidad de Productos a Agregar:", min_value=1, step=1, value=1, key="rentabilidad_num_productos")

    for i in range(int(num_productos)):
        st.subheader(f"Producto {i+1}")
        producto = st.text_input(f"Nombre del Producto {i+1}", key=f"rentabilidad_producto_{i}")
        ingreso = st.number_input(f"Ingresos del Producto {i+1} ($)", min_value=0.0, key=f"rentabilidad_ingreso_{i}")
        costo_fijo = st.number_input(f"Costos Fijos del Producto {i+1} ($)", min_value=0.0, key=f"rentabilidad_costo_fijo_{i}")
        costo_variable = st.number_input(f"Costos Variables del Producto {i+1} ($)", min_value=0.0, key=f"rentabilidad_costo_variable_{i}")
        data["Producto"].append(producto)
        data["Ingresos"].append(ingreso)
        data["Costos Fijos"].append(costo_fijo)
        data["Costos Variables"].append(costo_variable)

    # Guardar datos
    if st.button("Guardar Rentabilidad"):
        guardar_csv(data, filename)

    # Visualizar datos y gráficos
    df = pd.DataFrame(data)
    st.subheader("Datos de Rentabilidad")
    df["Rentabilidad"] = df["Ingresos"] - (df["Costos Fijos"] + df["Costos Variables"])
    st.dataframe(df)

    fig = px.bar(df, x="Producto", y="Rentabilidad", title="Rentabilidad por Producto")
    st.plotly_chart(fig)

# --- Sección de Pricing ---
def seccion_pricing():
    st.header("Cálculo de Pricing")
    filename = "pricing.csv"
    columnas = ["Producto", "Costos Fijos", "Costos Variables", "Precio de Venta"]

    # Inicializar datos
    df = cargar_csv(filename)
    data = df.to_dict(orient="list") if not df.empty else {"Producto": [], "Costos Fijos": [], "Costos Variables": [], "Precio de Venta": []}

    # Entrada de datos manual
    num_productos = st.number_input("Cantidad de Productos a Agregar:", min_value=1, step=1, value=1, key="pricing_num_productos")

    for i in range(int(num_productos)):
        st.subheader(f"Producto {i+1}")
        producto = st.text_input(f"Nombre del Producto {i+1}", key=f"pricing_producto_{i}")
        costo_fijo = st.number_input(f"Costos Fijos del Producto {i+1} ($)", min_value=0.0, key=f"pricing_costo_fijo_{i}")
        costo_variable = st.number_input(f"Costos Variables del Producto {i+1} ($)", min_value=0.0, key=f"pricing_costo_variable_{i}")
        margen = st.slider(f"Margen de Ganancia (%) para {producto}", 10, 100, 30, key=f"pricing_margen_{i}")
        precio_venta = (costo_fijo + costo_variable) * (1 + margen / 100)
        data["Producto"].append(producto)
        data["Costos Fijos"].append(costo_fijo)
        data["Costos Variables"].append(costo_variable)
        data["Precio de Venta"].append(precio_venta)

    # Guardar datos
    if st.button("Guardar Pricing"):
        guardar_csv(data, filename)

    # Visualizar datos y gráficos
    df = pd.DataFrame(data)
    st.subheader("Precios Calculados")
    st.dataframe(df)

    fig = px.bar(df, x="Producto", y=["Costos Fijos", "Costos Variables", "Precio de Venta"], title="Pricing por Producto")
    st.plotly_chart(fig)

# --- Menú Principal ---
menu = st.sidebar.radio("Seleccione una sección:", ["Flujo de Caja", "Rentabilidad", "Pricing"])

if menu == "Flujo de Caja":
    seccion_flujo_caja()
elif menu == "Rentabilidad":
    seccion_rentabilidad()
elif menu == "Pricing":
    seccion_pricing()