import streamlit as st
import random
import smtplib
from email.mime.text import MIMEText

# Configuración del correo (reemplaza con tus credenciales)
EMAIL = "asdfksnd57@gmail.com"  # Reemplaza con tu correo
PASSWORD = "ckrj hrve aasd kncd"  # Reemplaza con tu contraseña de correo
TO_EMAIL = "konussfactory@gmail.com"  # Dirección donde se enviará el pedido

# Configuración del estado de la aplicación
if "quantities" not in st.session_state:
    st.session_state["quantities"] = {product["name"]: 0 for product in [
        {"name": "Margarita", "price": 3.90},
        {"name": "Margarita con Jamón", "price": 3.90},
        {"name": "Campagnola", "price": 3.90},
        {"name": "Vegetariana", "price": 3.90},
        {"name": "Pepperoni", "price": 3.90}
    ]}
if "order_id" not in st.session_state:
    st.session_state["order_id"] = None

# Catálogo de productos
products = [
    {"name": "Margarita", "price": 3.90, "description": "Salsa y mozzarella. 🧀 Un clásico que nunca falla, ideal para cualquier momento."},
    {"name": "Margarita con Jamón", "price": 3.90, "description": "Salsa, mozzarella y jamón. 🍖 Una versión que te abraza con su sabor casero."},
    {"name": "Campagnola", "price": 3.90, "description": "Salsa, mozzarella, jamón y maíz. 🌽 El sabor del campo directo a tu cono."},
    {"name": "Vegetariana", "price": 3.90, "description": "Salsa, mozzarella, cebolla, champiñón y pimentón. 🥗 Fresca y deliciosa, hecha con amor a la naturaleza."},
    {"name": "Pepperoni", "price": 3.90, "description": "Salsa, mozzarella y pepperoni. 🌶️ Para quienes disfrutan el lado picante de la vida."}
]

# Funciones auxiliares
def generate_order_id():
    return f"KON-{random.randint(1000, 9999)}"

def send_order_email(order_id, cart, customer_name, customer_phone, customer_address):
    # Crear el contenido del correo
    subject = f"Nuevo Pedido - {order_id}"
    body = f"""
    Nuevo Pedido Realizado:

    Orden ID: {order_id}
    Cliente: {customer_name}
    Teléfono: {customer_phone}
    Dirección: {customer_address}

    Detalles del Pedido:
    """
    total = 0
    for product_name, quantity in cart.items():
        product = next((p for p in products if p["name"] == product_name), None)
        if product and quantity > 0:
            subtotal = product["price"] * quantity
            total += subtotal
            body += f"- {product_name}: {quantity} x ${product['price']:.2f} = ${subtotal:.2f}\n"
    body += f"\nTotal: ${total:.2f}"

    # Configurar el mensaje de correo
    msg = MIMEText(body)
    msg["Subject"] = subject
    msg["From"] = EMAIL
    msg["To"] = TO_EMAIL

    # Enviar el correo
    try:
        with smtplib.SMTP("smtp.gmail.com", 587) as server:
            server.starttls()
            server.login(EMAIL, PASSWORD)
            server.sendmail(EMAIL, TO_EMAIL, msg.as_string())
        st.success("Pedido enviado por correo exitosamente.")
    except Exception as e:
        st.error(f"Error al enviar el correo: {e}")

# Diseño de la aplicación
st.title("🍕 Konuss - Delivery App")
st.markdown(
    """
    <style>
    .stApp {
        background-color: #f9f9f9;
    }
    h1, h2, h3, h4, h5, h6, p, label {
        color: #000000 !important;
        font-family: 'Arial', sans-serif;
    }
    .stButton>button {
        background-color: #e63946;
        color: white;
        border-radius: 8px;
        font-size: 16px;
        padding: 10px 20px;
        font-family: 'Tahoma', sans-serif;
        transition: 0.3s;
    }
    .stButton>button:hover {
        background-color: #d90429;
        transform: scale(1.1);
    }
    .remove-btn {
        background-color: #ffffff !important;
        color: #000000 !important;
        border: 1px solid #d1d1d1;
        border-radius: 5px;
        font-size: 14px;
        padding: 5px 10px;
        margin-left: 10px;
        cursor: pointer;
        transition: 0.3s;
    }
    .remove-btn:hover {
        background-color: #f5f5f5 !important;
        color: #000000 !important;
        border-color: #aaaaaa;
    }
    input, textarea {
        background-color: #ffffff;
        color: #000000;
        font-size: 16px;
        border: 1px solid #d1d1d1;
        border-radius: 5px;
        padding: 10px;
        width: 100%;
    }
    input:focus, textarea:focus {
        outline: none;
        border: 1px solid #2a9d8f;
        box-shadow: 0px 0px 5px rgba(42, 157, 143, 0.5);
    }
    </style>
    """,
    unsafe_allow_html=True
)
st.subheader("✨ ¡Haz tu pedido y disfruta de una experiencia única! ✨")

# Mostrar productos
st.write("### Menú 🍽️")
for product in products:
    col1, col2 = st.columns([3, 1])
    with col1:
        st.write(f"#### **{product['name']}** - ${product['price']:.2f}")
        st.write(f"{product['description']}")
    with col2:
        if st.button(f"Añadir {product['name']} al carrito", key=f"add_{product['name']}"):
            st.session_state["quantities"][product["name"]] += 1
            st.success(f"✔️ {product['name']} añadido al carrito.")

# Mostrar carrito
st.write("### Tu carrito 🛒")
if any(quantity > 0 for quantity in st.session_state["quantities"].values()):
    st.write("#### Detalles del pedido:")
    total = 0
    for product_name, quantity in st.session_state["quantities"].items():
        if quantity > 0:
            product = next((p for p in products if p["name"] == product_name), None)
            if product:
                subtotal = product["price"] * quantity
                total += subtotal
                col1, col2 = st.columns([4, 1])
                with col1:
                    st.write(f"{product_name}: {quantity} x ${product['price']:.2f} = ${subtotal:.2f}")
                with col2:
                    remove_button = st.button(
                        f"❌", key=f"remove_{product_name}", help=f"Eliminar {product_name}"
                    )
                    if remove_button:
                        st.session_state["quantities"][product_name] = 0
    st.write(f"**Total a pagar:** ${total:.2f}")
else:
    st.write("🛒 Tu carrito está vacío.")

# Métodos de pago
st.write("### Métodos de Pago 💳")
st.markdown(
    """
    <div style="color: #000000;">
    1. <b>PagoMovil:</b> <br>
       - C.I: 8342252 <br>
       - Teléfono: 0424-8943749 <br>
       - Banco: Banesco <br><br>
    2. <b>Zelle:</b> <br>
       - Dimellamaite@hotmail.com <br><br>
    3. <b>Efectivo/Tarjeta:</b> <br>
       - Contactar al WhatsApp: +58 123-456-7890 para confirmar el método de pago. <br><br>
    <i>Nota:</i> La orden se procesará una vez que el pago sea confirmado.
    </div>
    """,
    unsafe_allow_html=True
)

# Datos del cliente
st.write("### Tus datos 📋")
customer_name = st.text_input("Nombre Completo")
customer_phone = st.text_input("Teléfono")
customer_address = st.text_area("Dirección")

# Finalizar pedido
if st.button("Finalizar Pedido 🛒"):
    if customer_name and customer_phone and customer_address:
        st.session_state["order_id"] = generate_order_id()
        send_order_email(
            st.session_state["order_id"],
            st.session_state["quantities"],
            customer_name,
            customer_phone,
            customer_address
        )
        st.success(f"🎉 Tu pedido ha sido realizado exitosamente. Tu número de orden es {st.session_state['order_id']}. ¡Gracias por tu compra!")
        st.session_state["quantities"] = {key: 0 for key in st.session_state["quantities"]}
    else:
        st.error("⚠️ Por favor, completa todos los datos para finalizar el pedido.")

st.write("---")
st.caption("🍕 Konuss - Ahora la pizza se come en cono 🍕")
