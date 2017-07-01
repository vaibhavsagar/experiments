all: pdf html readme

pdf: resume.md templates/header.tex
	pandoc resume.md -H templates/header.tex -o Vaibhav_Sagar_resume.pdf

html: resume.md templates/header.css
	pandoc resume.md -s -H templates/header.css -o index.html

readme: resume.md
	pandoc resume.md -t markdown_github -o readme.md

clean:
	rm Vaibhav_Sagar_resume.pdf
	rm readme.md
	rm index.html
