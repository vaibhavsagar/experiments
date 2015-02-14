all: pdf html readme

pdf: resume.md resume_header.tex
	pandoc resume.md -H resume_header.tex -o Vaibhav_Sagar_resume.pdf

html: resume.md resume_template.css
	pandoc resume.md -s -H resume_template.css -o index.html

readme: resume.md
	pandoc resume.md -t markdown_github -o readme.md

clean:
	rm Vaibhav_Sagar_resume.pdf
	rm readme.md
	rm index.html
