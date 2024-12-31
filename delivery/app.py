import streamlit as st
import random
import smtplib
from email.mime.text import MIMEText

# Configuración del correo
EMAIL = "asdfksnd57@gmail.com"
PASSWORD = "ckrj hrve aasd kncd"
TO_EMAIL = "konussfactory@gmail.com"

# Configuración inicial del estado
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
if "total" not in st.session_state:
    st.session_state["total"] = 0.0

# Catálogo de productos
products = [
    {"name": "Margarita", "price": 3.90, "description": "🍅 Salsa y mozzarella. Un clásico irresistible."},
    {"name": "Margarita con Jamón", "price": 3.90, "description": "🍖 Salsa, mozzarella y jamón. Delicioso."},
    {"name": "Campagnola", "price": 3.90, "description": "🌽 Salsa, mozzarella, jamón y maíz. Sabores únicos."},
    {"name": "Vegetariana", "price": 3.90, "description": "🥬 Salsa, mozzarella, cebolla y champiñones. Fresca."},
    {"name": "Pepperoni", "price": 3.90, "description": "🍕 Salsa, mozzarella y pepperoni. La que nunca falla."}
]

# Función para generar un ID único de pedido
def generate_order_id():
    return f"KON-{random.randint(1000, 9999)}"

# Función para enviar el pedido por correo
def send_order_email(order_id, cart, customer_name, customer_phone, customer_address):
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

    msg = MIMEText(body)
    msg["Subject"] = subject
    msg["From"] = EMAIL
    msg["To"] = TO_EMAIL

    try:
        with smtplib.SMTP("smtp.gmail.com", 587) as server:
            server.starttls()
            server.login(EMAIL, PASSWORD)
            server.sendmail(EMAIL, TO_EMAIL, msg.as_string())
        st.success("¡Pedido enviado por correo exitosamente! 🚀")
    except Exception as e:
        st.error(f"❌ Error al enviar el correo: {e}")

# Estilo CSS actualizado
st.markdown("""
    <style>
        .stApp {
            background: linear-gradient(to bottom, #ffffff, #f9f9f9);
            font-family: 'Poppins', sans-serif;
            color: #000000 !important;
        }
        .header {
            text-align: center;
            color: #e63946;
            font-size: 36px;
            font-weight: bold;
            margin-bottom: 10px;
        }
        .subheader {
            text-align: center;
            color: #495057;
            font-size: 20px;
            margin-bottom: 30px;
        }
        .section-title {
            background: #e63946;
            color: white;
            font-size: 20px;
            font-weight: bold;
            text-align: center;
            padding: 12px;
            border-radius: 12px;
            margin-bottom: 20px;
        }
        .add-button {
            background-color: #e63946 !important;
            color: white !important;
            border-radius: 8px !important;
            padding: 10px !important;
            font-size: 16px !important;
            cursor: pointer !important;
            border: none !important;
            box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1) !important;
            transition: all 0.3s ease-in-out !important;
        }
        .add-button:hover {
            background-color: #c22834 !important;
        }
        input, textarea {
            background-color: #ffffff !important;
            color: #000000 !important;
            border: 2px solid #e63946 !important;
            border-radius: 8px !important;
            padding: 10px !important;
            font-size: 16px !important;
        }
        input:focus, textarea:focus {
            outline: none !important;
            border: 2px solid #c22834 !important;
            box-shadow: 0px 0px 5px rgba(226, 57, 70, 0.5) !important;
        }
    </style>
""", unsafe_allow_html=True)

# Encabezado
st.markdown("<h1 class='header'>🍕 Konuss - ¡Ahora la pizza se come en cono! 🎉</h1>", unsafe_allow_html=True)
st.markdown("<h2 class='subheader'>✨ ¡Haz tu pedido y disfruta de una experiencia única! ✨</h2>", unsafe_allow_html=True)

# Sección Menú
st.markdown("<div class='section-title'>📋 Menú</div>", unsafe_allow_html=True)
for product in products:
    st.write(f"**{product['name']}** - ${product['price']:.2f}")
    st.write(f"{product['description']}")
    if st.button(f"Añadir {product['name']} al carrito", key=f"add_{product['name']}"):
        st.session_state["quantities"][product["name"]] += 1
        st.success(f"🎉 ¡{product['name']} añadido al carrito!")

# Sección Carrito
st.markdown("<div class='section-title'>🛒 Tu carrito</div>", unsafe_allow_html=True)
if any(quantity > 0 for quantity in st.session_state["quantities"].values()):
    st.session_state["total"] = 0
    for product_name, quantity in st.session_state["quantities"].items():
        if quantity > 0:
            product = next((p for p in products if p["name"] == product_name), None)
            if product:
                col1, col2, col3 = st.columns([2, 2, 1])
                with col1:
                    st.write(f"**{product_name}** - ${product['price']:.2f} c/u")
                with col2:
                    new_quantity = st.number_input(
                        f"Cantidad ({product_name})",
                        min_value=0,
                        max_value=100,
                        value=quantity,
                        step=1,
                        key=f"quantity_{product_name}"
                    )
                    st.session_state["quantities"][product_name] = new_quantity
                with col3:
                    subtotal = product['price'] * new_quantity
                    st.session_state["total"] += subtotal
                    st.write(f"${subtotal:.2f}")
    st.write(f"### Total: ${st.session_state['total']:.2f} 💵")
else:
    st.write("¡Tu carrito está vacío! 😢")

# Sección Datos del Cliente
st.markdown("<div class='section-title'>🚀 Finalizar Pedido</div>", unsafe_allow_html=True)
customer_name = st.text_input("📝 Nombre Completo")
customer_phone = st.text_input("📞 Teléfono")
customer_address = st.text_area("📍 Dirección")
if st.button("Confirmar Pedido ✅"):
    if customer_name and customer_phone and customer_address:
        st.session_state["order_id"] = generate_order_id()
        send_order_email(
            st.session_state["order_id"],
            st.session_state["quantities"],
            customer_name,
            customer_phone,
            customer_address
        )
        st.success(f"¡Pedido enviado! Orden ID: {st.session_state['order_id']} 🚀. Por favor, compartir comprobante de pago con el número de orden al Whatsapp +58 0424-8943749 o al e-mail konussfactory@gmail.com. ⚠️El pedido será enviado una vez el pago haya sido confirmado.⚠️")
    else:
        st.error("⚠️ Por favor, completa todos los campos.")
