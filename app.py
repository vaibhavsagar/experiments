from flask import Flask, request
from os import environ
app = Flask(__name__)
port = int(environ.get('PORT', 5000))

content = """
<body style="width: 1080px; height: 1920px; margin: 0; position: absolute; top: 0; left: 0;">
    <meta name="viewport" content="width=1080, height=1920, initial-scale=1">
    <iframe
        id="main"
        src="{}"
        frameborder=0
        width=1920
        height=1080
        style="transform: rotate(90deg) translate(26em, 26em)" target="_parent">
    </iframe>
</body>"""


@app.route('/')
def main():
    url = request.args.get('url', '')
    if url:
        return content.format(url)
    return "Hello, World!"

if __name__ == "__main__":
    app.run(host="0.0.0.0", port=port)
