all: pdf html readme

pdf: resume.md templates/resume_header.tex
	pandoc resume.md -H templates/resume_header.tex -o Vaibhav_Sagar_resume.pdf

html: resume.md templates/resume_template.css
	pandoc resume.md -s -H templates/resume_template.css -o index.html

readme: resume.md
	pandoc resume.md -t markdown_github -o readme.md

clean:
	rm Vaibhav_Sagar_resume.pdf
	rm readme.md
	rm index.html
