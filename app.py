from flask import Flask, request
app = Flask(__name__)


@app.route('/')
def main():
    url = request.args.get('url', '')
    if url:
        return """<iframe id="main" src="{}" frameborder=0 width=1920 height=1080
            style="-webkit-transform: rotate(90deg);" target="_parent"></iframe>""".format(url)
    return "Hello, World!"

if __name__ == "__main__":
    app.run(host="0.0.0.0")
