import streamlit as st
import pandas as pd
import plotly.express as px
import os
from fpdf import FPDF
from io import BytesIO

# --- Configuración de la página ---
st.set_page_config(page_title="SDM Advisory - Herramienta Básica", layout="wide")

# --- Funciones Auxiliares ---
def cargar_datos(filename, columnas):
    """Carga datos desde un archivo CSV o crea un DataFrame vacío con las columnas requeridas."""
    if os.path.exists(filename):
        df = pd.read_csv(filename)
        for col in columnas:
            if col not in df.columns:
                df[col] = [0] * len(df)  # Inicializa columnas faltantes con ceros
        return df
    else:
        return pd.DataFrame(columns=columnas)

def generar_pdf(df, titulo, explicacion):
    """Genera un PDF con los datos y la interpretación en palabras."""
    pdf = FPDF()
    pdf.add_page()
    pdf.set_font("Arial", size=12)

    # Título del análisis
    pdf.set_font("Arial", style="B", size=16)
    pdf.cell(200, 10, txt=titulo, ln=True, align="C")
    pdf.ln(10)

    # Explicación en palabras
    pdf.set_font("Arial", size=12)
    pdf.multi_cell(0, 10, explicacion)
    pdf.ln(10)

    # Tabla de datos
    pdf.set_font("Arial", style="B", size=12)
    pdf.cell(0, 10, txt="Datos Analizados:", ln=True)
    pdf.set_font("Arial", size=10)

    # Encabezados
    col_width = 190 // len(df.columns)  # Calcular el ancho de columnas
    for col in df.columns:
        pdf.cell(col_width, 10, txt=str(col), border=1, align="C")
    pdf.ln()

    # Filas de datos
    for _, row in df.iterrows():
        for item in row:
            pdf.cell(col_width, 10, txt=str(item), border=1, align="C")
        pdf.ln()

    # Guardar el PDF en memoria
    buffer = BytesIO()
    pdf.output(buffer, 'S')
    buffer.seek(0)

    return buffer

def guardar_datos(df, filename):
    """Guarda un DataFrame en un archivo CSV."""
    df.to_csv(filename, index=False)
    st.success(f"Datos guardados exitosamente en {filename}.")

def reiniciar_datos(filename, columnas):
    """Reinicia los datos eliminando el archivo CSV y volviendo a un DataFrame vacío."""
    if os.path.exists(filename):
        os.remove(filename)
        st.success("Datos reiniciados. El archivo CSV ha sido eliminado.")
    return pd.DataFrame(columns=columnas)

def ingreso_datos_manual(columnas, filename):
    """Permite ingresar datos manualmente."""
    st.subheader("Ingreso Manual de Datos")
    df = cargar_datos(filename, columnas)

    # Crear DataFrame temporal para agregar nuevas filas
    nuevas_filas = []

    # Ingreso de nuevas filas
    num_filas = st.number_input("Cantidad de filas a agregar:", min_value=1, step=1, key=f"{filename}_filas")
    for i in range(num_filas):
        st.write(f"Fila {i + 1}")
        fila = {}
        for col in columnas:
            if col == "Producto" or col == "Fecha":
                valor = st.text_input(f"{col}:", key=f"{filename}_{col}_{i}")
            else:
                valor = st.number_input(f"{col}:", min_value=0.0, key=f"{filename}_{col}_{i}")
            fila[col] = valor
        nuevas_filas.append(fila)

    # Agregar nuevas filas al DataFrame original
    if nuevas_filas:
        nuevas_filas_df = pd.DataFrame(nuevas_filas)
        df = pd.concat([df, nuevas_filas_df], ignore_index=True)

    # Guardar datos
    if st.button("Guardar Datos Manuales", key=f"{filename}_guardar"):
        guardar_datos(df, filename)
        st.write("### Datos Guardados")
        st.dataframe(df)

    return df

# --- Menú Principal ---
st.title("SDM Advisory - Herramienta desde Cero")

menu = st.sidebar.radio("Seleccione un servicio:", ["Flujo de Caja", "Rentabilidad por Unidad", "Pricing con BEP", "Reiniciar Datos"])

# --- Flujo de Caja ---
if menu == "Flujo de Caja":
    st.header("Flujo de Caja")
    columnas = ["Fecha", "Ingresos", "Egresos"]
    filename = "flujo_caja.csv"
    df = cargar_datos(filename, columnas)

    if st.checkbox("Cargar datos desde CSV"):
        archivo = st.file_uploader("Sube un archivo CSV con las columnas: Fecha, Ingresos, Egresos", type=["csv"])
        if archivo:
            df = pd.read_csv(archivo)
            guardar_datos(df, filename)

    elif st.checkbox("Ingresar datos manualmente"):
        df = ingreso_datos_manual(columnas, filename)

    if df.empty:
        st.error("El DataFrame está vacío. Por favor, ingrese datos manualmente o cargue un archivo CSV.")
    else:
        # Realizar cálculos y mostrar resultados
        df["Flujo Neto"] = df["Ingresos"] - df["Egresos"]
        df["Flujo Acumulado"] = df["Flujo Neto"].cumsum()
        st.write("### Resultados del Análisis")
        st.dataframe(df)
        fig1 = px.bar(df, x="Fecha", y=["Ingresos", "Egresos", "Flujo Neto"], title="Flujo de Caja: Ingresos vs Egresos")
        fig2 = px.line(df, x="Fecha", y="Flujo Acumulado", title="Flujo Neto Acumulado")
        st.plotly_chart(fig1)
        st.plotly_chart(fig2)

        # Botón para generar el PDF
        if st.button("Generar PDF"):
            titulo = "Análisis de Flujo de Caja"
            explicacion = (
                f"Este análisis detalla los ingresos, egresos y flujo neto acumulado. "
                f"El total de ingresos es de {df['Ingresos'].sum():,.2f}, mientras que los egresos suman {df['Egresos'].sum():,.2f}. "
                f"El flujo neto acumulado es de {df['Flujo Acumulado'].iloc[-1]:,.2f}."
            )
            pdf_buffer = generar_pdf(df, titulo, explicacion)
            st.download_button(
                label="Descargar Análisis en PDF",
                data=pdf_buffer,
                file_name="analisis_flujo_caja.pdf",
                mime="application/pdf"
            )

# --- Rentabilidad por Unidad ---
elif menu == "Rentabilidad por Unidad":
    st.header("Rentabilidad por Unidad")
    columnas = ["Producto", "Unidades Producidas", "Precio Unitario", "Costos Fijos por Unidad", "Costos Variables por Unidad"]
    filename = "rentabilidad.csv"
    df = cargar_datos(filename, columnas)

    if st.checkbox("Cargar datos desde CSV"):
        archivo = st.file_uploader("Sube un archivo CSV con las columnas: Producto, Unidades Producidas, Precio Unitario, Costos Fijos por Unidad, Costos Variables por Unidad", type=["csv"])
        if archivo:
            df = pd.read_csv(archivo)
            guardar_datos(df, filename)

    elif st.checkbox("Ingresar datos manualmente"):
        df = ingreso_datos_manual(columnas, filename)

    if df.empty:
        st.error("El DataFrame está vacío. Por favor, ingrese datos manualmente o cargue un archivo CSV.")
    else:
        df["Ingresos Totales"] = df["Unidades Producidas"] * df["Precio Unitario"]
        df["Costos Totales"] = (df["Costos Fijos por Unidad"] + df["Costos Variables por Unidad"]) * df["Unidades Producidas"]
        df["Rentabilidad"] = df["Ingresos Totales"] - df["Costos Totales"]
        df["Margen (%)"] = (df["Rentabilidad"] / df["Ingresos Totales"]) * 100
        st.write("### Resultados del Análisis")
        st.dataframe(df)
        fig1 = px.bar(df, x="Producto", y=["Ingresos Totales", "Costos Totales", "Rentabilidad"], title="Rentabilidad por Producto")
        fig2 = px.histogram(df, x="Margen (%)", nbins=10, title="Distribución de Márgenes")
        st.plotly_chart(fig1)
        st.plotly_chart(fig2)
